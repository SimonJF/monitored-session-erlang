-module(conversation_instance).
-compile(export_all).
-behaviour(gen_server2).

-record(subsession_state, {initiator_pid,
                           initiator_protocol,
                           initiator_role,
                           parent_conv_id,
                           root_conv_id
                          }).

-record(conv_inst_state, {protocol_name,             %% Name of the protocol
                          role_mapping,              %% Role |-> Endpoint mapping
                          participant_monitor_refs,  %% Monitor refs for roles:
                                                     %% used in push-based FD
                          conv_properties,           %% Per-conv properties.
                          subsession_state           %% Extra info if this is a
                                                     %% subsession
                         }).

% Essentially a routing table for the conversation instance.
% Needs to have functionality for actor-role discovery.

% State:
% Protocol Name
% Role |-> Endpoint Mapping

log_msg(Func, Format, Args, State) ->
  InfoStr = "(Conversation for protocol ~s, CID ~s)",
  InfoArgs = [State#conv_inst_state.protocol_name, self()],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

conversation_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

conversation_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

conversation_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).


% Creates an Erlang monitor for the participant. Not a MST monitor!
% This helps us do push-based failure detection.
% We get a monitor reference back, and whenever the process dies,
% we receive a DOWN notification (which we trap).
monitor_participants(RoleEndpointList, State) ->
  MonitorRefList =
    lists:foldr(fun({Role, Endpoint}, Refs) ->
                    MonitorRef = erlang:monitor(process, Endpoint),
                    [{Role, MonitorRef}|Refs] end, [], RoleEndpointList),
  State#conv_inst_state{participant_monitor_refs=orddict:from_list(MonitorRefList)}.

% Checks whether the role is transient in the rolespec or not.
% Initial val is not_filled if it isn't, and not_filled_transient if it is.
initial_filled_val({RoleName, {local_protocol, _, _, _, Roles, _}}) ->
  lists:foldl(fun({Ty, RN}, Acc) ->
                  if RN == RoleName andalso Ty == transient_role_decl ->
                       not_filled_transient;
                     true -> Acc
                  end end, not_filled, Roles).


fresh_state(ProtocolName) ->
  #conv_inst_state{protocol_name=ProtocolName, role_mapping=orddict:new(),
                   participant_monitor_refs=orddict:new(),
                   conv_properties=orddict:new(),
                   subsession_state=undefined
                  }.

fresh_state(ProtocolName, ParentConvID, InitiatorPID, InitiatorProtocol, InitiatorRole) ->
  RootPID = conversation_instance:get_root_pid(ParentConvID),
  State = fresh_state(ProtocolName),
  SubsessionState = #subsession_state{initiator_pid=InitiatorPID,
                                      initiator_protocol=InitiatorProtocol,
                                      initiator_role=InitiatorRole,
                                      parent_conv_id=ParentConvID,
                                      root_conv_id=RootPID
                                     },
  State#conv_inst_state{subsession_state=SubsessionState}.

broadcast_end_notification(Reason, State) ->
  % Only send one notification per actor.
  RoleMappingList = orddict:to_list(State#conv_inst_state.role_mapping),
  PIDs = lists:map(fun({_, Pid}) -> Pid end, RoleMappingList),
  UniqList = sets:to_list(sets:from_list(PIDs)),
  lists:foreach(fun(Pid) -> actor_monitor:conversation_ended(Pid, self(), Reason) end,
                UniqList).

handle_get_root_pid(State) ->
  SubsessionState = State#conv_inst_state.subsession_state,
  if SubsessionState =/= undefined ->
       SubsessionState#subsession_state.root_conv_id;
     SubsessionState == undefined ->
       % This is the root
       self()
  end.

handle_end_conversation(Reason, State) ->
  broadcast_end_notification(Reason, State),
  exit(normal),
  {noreply, State}.

% Given an actor-role mapping, pings each actor to check if alive or dead.
get_role_alive_status(RoleEndpointMappingList) ->
  get_role_alive_status_inner(RoleEndpointMappingList, {[], []}).

get_role_alive_status_inner([], {Alive, Dead}) ->
  {Alive, Dead};
get_role_alive_status_inner([{R, E}|XS], {Alive, Dead}) ->
  ActorAlive = actor_monitor:is_actor_alive(E),
  if ActorAlive ->
       get_role_alive_status_inner(XS, {[R|Alive], Dead});
     not ActorAlive ->
       get_role_alive_status_inner(XS, {Alive, [R|Dead]})
  end.

check_role_usage([], _) -> not_used;
check_role_usage([{Role, Endpoint}|XS], DownRole)
  when DownRole =/= Role ->
  case (catch(actor_monitor:check_role_reachable(Endpoint, self(),
                                                 Role, DownRole))) of
    % If it's reachable, no need to check the rest, report
    true -> {used, Role};
    % If not, we need to check the rest
    false -> check_role_usage(XS, DownRole);
    _Other -> {down, Role}
  end;
check_role_usage([_|XS], DownRole) ->
  check_role_usage(XS, DownRole).

handle_begin_continuation_safety_check(MonitorRef, State) ->
  RoleMapping = State#conv_inst_state.role_mapping,
  RoleEndpoints = orddict:to_list(RoleMapping),
  ParticipantMonitorRefs = State#conv_inst_state.participant_monitor_refs,
  case orddict:find(MonitorRef, ParticipantMonitorRefs) of
    {ok, RoleName} ->
      error_logger:info_msg("Role ~p down. Beginning safety check.~n", [RoleName]),

      % Iterate over each -- should a node fail, ignore it, it'll be picked up later
      % If UsedInRemainder, there's at least one actor which needs to communicate
      % with the dead role in the remainder of the session.
      % If the down role is used in the remainder, or a call failed, we abort the
      % session.
      % If it's not, however, it's fine to proceed.
      case check_role_usage(RoleEndpoints, RoleName) of
        not_used ->
          error_logger:info_msg("Role ~p passed safety check: unused in all other participants.~n",
                                [RoleName]),
          ok; % We're golden
        {used, ReferencerRole} ->
          error_logger:info_msg("Safety check failed: ~p still needed in ~p~n", [RoleName, ReferencerRole]),
          end_conversation(self(), {role_offline, RoleName});
        {down, DownRole} ->
          error_logger:info_msg("Safety check failed: Unable to contact ~p~n", [DownRole]),
          end_conversation(self(), safety_check_failed)
      end;
    error ->
      % Couldn't find in the participant monitor refs. Probably something different.
      exit(unknown_monitor_offline),
      ok
  end.

get_endpoints(RoleList, ParentRoleMapping) ->
  lists:map(fun(Role) -> {Role, orddict:fetch(Role, ParentRoleMapping)} end, RoleList).

split_external_invitations([], Known, Unknown) ->
  {Known, Unknown};
split_external_invitations([X={_Role, _Endpoint}|XS], Known, Unknown) ->
  split_external_invitations(XS, [X|Known], Unknown);
split_external_invitations([X|XS], Known, Unknown) ->
  split_external_invitations(XS, Known, [X|Unknown]).

% InternalRoles : [Role]
% ExternalInvitations : [{Role, Endpoint}]
handle_send_subsession_invitations(InternalRoles, ExternalInvitations, ParentRoleMapping,
                                   State) ->
  % Load everything we need from the state
  SubsessionState = State#conv_inst_state.subsession_state,
  InitiatorPID = SubsessionState#subsession_state.initiator_pid,
  InitiatorProtocol = SubsessionState#subsession_state.initiator_protocol,
  InitiatorRole = SubsessionState#subsession_state.initiator_role,
  ParentConvID = SubsessionState#subsession_state.parent_conv_id,
  ProtocolName = State#conv_inst_state.protocol_name,

  % Get the inhabitants of all internally-invited roles
  InternalInvitations = get_endpoints(InternalRoles, ParentRoleMapping),

  {KnownExternalEndpoints, UnknownExternalEndpoints} =
    split_external_invitations(ExternalInvitations, [], []),

  % InternalInvitations, ExternalInvitations: [{Role, Endpoint}]
  InvitationList = InternalInvitations ++ KnownExternalEndpoints,
  RoleMonitorMap = protocol_registry:get_roles_monitors(ProtocolName),
  UnknownExternalInviteRes =
    invite_actor_subset(UnknownExternalEndpoints, RoleMonitorMap, State),

  case UnknownExternalInviteRes of
    {ok, DiscoveredSuccesses} ->
      error_logger:info_msg("OK, DiscoveredSuccesses: ~p~n", [DiscoveredSuccesses]),
      KnownInviteRes = invite_known_actors(InvitationList, RoleMonitorMap, ProtocolName, []),
      case KnownInviteRes of
        {ok, KnownSuccesses} ->
          RoleList = DiscoveredSuccesses ++ KnownSuccesses,
          error_logger:info_msg("Everything is good!: ~p~n", [RoleList]),
          notify_session_setup_success(ProtocolName, RoleList, orddict:from_list(RoleList)),
          NewState1 = monitor_participants(RoleList, State),
          NewState2 = NewState1#conv_inst_state{role_mapping=orddict:from_list(RoleList)},
          {ok, NewState2};
        {error, Err, ToNotify} ->
          notify_session_setup_fail(ProtocolName, ToNotify ++ DiscoveredSuccesses),
          actor_monitor:subsession_setup_failed(InitiatorPID, ProtocolName, self(),
                                                InitiatorProtocol, InitiatorRole,
                                                ParentConvID, invitation_failed),

          {{error, Err}, State}
      end;
    {error, Err, ToNotify} ->
       notify_session_setup_fail(ProtocolName, ToNotify),
       actor_monitor:subsession_setup_failed(InitiatorPID, ProtocolName, self(),
                                             InitiatorProtocol, InitiatorRole,
                                             ParentConvID, invitation_failed),

       {{error, Err}, State}
  end.


invite_known_actors([], _, _, Successes) ->
  {ok, Successes};
invite_known_actors([{Role, Endpoint}|XS], Monitors, ProtocolName, Successes) ->
  % TODO: Error check this, user might not have entered available roles
  Monitor = orddict:fetch(Role, Monitors),
  try actor_monitor:incoming_invitation(Endpoint, ProtocolName, Role, self(), Monitor) of
    ok ->
      invite_known_actors(XS, Monitors, ProtocolName, [{Role, Endpoint}|Successes])
    catch
      _:Err ->
        error_logger:error_msg("Subsession invitation of role ~p failed: ~p~n",
                               [Role, Err]),
        {error, {internal_refused, Role}, Successes}
  end.



%%%%% Invitation stuff
invite_actor_subset(RoleNames, RoleMonitorMap, State) ->
  ProtocolName = State#conv_inst_state.protocol_name,
  % Get the actor PIDs able to fulfil each role
  RoleActorMap = actor_registry:get_actors_for(ProtocolName),
  JoinedMapRes = join_maps(RoleMonitorMap, RoleActorMap),
  case JoinedMapRes of
    {ok, JoinedMaps} ->
      FilteredJoinedMapRes = lists:filter(fun({Role, _, _}) ->
                                          lists:member(Role, RoleNames) end, JoinedMaps), % ugh O(n^2)
      invite_actors_inner(FilteredJoinedMapRes, undefined, undefined, [], State); % hackity hack...
    _Err -> {{error, actors_unregistered}, State}
  end.



invite_actors(InitiatorRole, InitiatorPID, State) ->
  ProtocolName = State#conv_inst_state.protocol_name,
  RoleMonitorMap = protocol_registry:get_roles_monitors(ProtocolName),
  % Get the actor PIDs able to fulfil each role
  RoleActorMap = actor_registry:get_actors_for(ProtocolName),

  % Join the maps together. That is, [{Role, Monitor}] + [{Role, [ActorPID]}] =
  % [{Role, Monitor, [ActorPID]}].
  JoinedMapRes = join_maps(RoleMonitorMap, RoleActorMap),
  case JoinedMapRes of
    {ok, JoinedMaps} ->
      InviteRes = invite_actors_inner(JoinedMaps, InitiatorRole, InitiatorPID, [], State),
      case InviteRes of
        {ok, Successes} ->
          RoleDict = orddict:from_list(Successes),
          notify_session_setup_success(ProtocolName, Successes, RoleDict),
          NewState1 = monitor_participants(Successes, State),
          NewState2 = NewState1#conv_inst_state{role_mapping=RoleDict},
          {ok, NewState2};
        {error, Err, ToNotify} ->
          notify_session_setup_fail(ProtocolName, ToNotify),
          {{error, Err}, State}
      end;
    _Err ->
      % Join mismatch: some roles in the R-M table don't have registered actors associated
      {{error, actors_unregistered}, State}
  end.

notify_session_setup_success(ProtocolName, ToNotify, RoutingTable) ->
  lists:foreach(fun({R, PID}) ->
                    actor_monitor:conversation_success(PID, ProtocolName, R, self(),
                                                       RoutingTable) end,
                ToNotify).


notify_session_setup_fail(ProtocolName, ToNotify) ->
  lists:foreach(fun({R, PID}) ->
                    actor_monitor:conversation_setup_failed(PID, ProtocolName, R, setup) end,
                ToNotify),
  end_conversation(self(), invitation_failed).


% [{Role, Monitor, ActorPIDs}], Successes: [{Role, ActorID}], State.
invite_actors_inner([], _, _, Successes, _State) -> {ok, Successes};
invite_actors_inner([{Role, Monitor, _}|XS], InitiatorRole, InitiatorPID,
                    Successes, State) when Role == InitiatorRole ->
  Protocol = State#conv_inst_state.protocol_name,
  % If it's the initiator role, we need to invite the initiator directly
  try actor_monitor:incoming_invitation(InitiatorPID, Protocol, Role, self(), Monitor) of
    ok -> % Great!
      invite_actors_inner(XS, InitiatorRole, InitiatorPID,
                          [{InitiatorRole, InitiatorPID}|Successes], State);
    {error, Err} ->
      error_logger:error_msg("Could not invite initiator, error: ~p~n", [Err]),
      {error, initiator_refused, Successes}
  catch
    _:Err ->
      error_logger:error_msg("Could not invite initiator.Err:~p~n", [Err]),
      {error, initiator_unreachable, Successes}
  end;
invite_actors_inner([{Role, Monitor, ActorPIDs}|XS], InitiatorRole,
                    InitiatorPID, Successes, State) ->
  Protocol = State#conv_inst_state.protocol_name,
  InviteRes = invite_actor(ActorPIDs, Protocol, Role, Monitor),
  case InviteRes of
    {ok, PID} -> invite_actors_inner(XS, InitiatorRole, InitiatorPID,
                                     [{Role, PID}|Successes], State);
    error ->
      error_logger:error_msg("Could not invite an actor to fulfil ~p.~n", [Role]),
      {error, {could_not_fulfil, Role}, Successes}
  end.

invite_actor([], _, _, _) -> error;
invite_actor([PID|PIDs], Protocol, Role, Monitor) ->
  % Try to invite the actor
  % incoming_invitation(MonitorPID, ProtocolName, RoleName, ConversationID) ->
  try actor_monitor:incoming_invitation(PID, Protocol, Role, self(), Monitor) of
      ok -> {ok, PID};
      {error, Err} ->
        error_logger:info_msg("Couldn't invite ~p to fulfil role ~p in ~p, error: ~n",
                              [PID, Role, Protocol, Err]),
        invite_actor(PIDs, Protocol, Role, Monitor)
  catch
    _:_Err ->
      error_logger:info_msg("Couldn't invite ~p to fulfil role ~p in ~p~n",
                            [PID, Role, Protocol]),
      invite_actor(PIDs, Protocol, Role, Monitor)
  end.


join_maps(RMM, RAM) ->
  if length(RMM) =/= length(RAM) ->
       error;
     true ->
       SortedRMM = lists:keysort(1, RMM),
       SortedRAM = lists:keysort(1, RAM),
       Res = join_maps_inner(SortedRMM, SortedRAM),
       case Res of
         {error, bad_join} -> {error, bad_join};
         Success -> {ok, Success}
       end
  end.

join_maps_inner([], _) -> [];
join_maps_inner([{Role1, Monitor}|XS], [{Role2, ActorPIDs}|YS]) when Role1 == Role2 ->
  [{Role1, Monitor, ActorPIDs}|join_maps_inner(XS, YS)];
join_maps_inner(_, _) ->
  {error, bad_join}.


%%%% Subsession stuff


get_subsession_state(State) ->
  State#conv_inst_state.subsession_state.

handle_subsession_end(EndType, Arg, State) ->
% Broadcast session end message to all participants
  broadcast_end_notification(normal, State),
  % Send "subsession complete" notification to initiator
  SubsessionState = get_subsession_state(State),
  SubsessionName = State#conv_inst_state.protocol_name,
  InitiatorPID = SubsessionState#subsession_state.initiator_pid,
  InitiatorPN = SubsessionState#subsession_state.initiator_protocol,
  InitiatorRN = SubsessionState#subsession_state.initiator_role,
  InitiatorCID = SubsessionState#subsession_state.parent_conv_id,
  if EndType == success ->
       actor_monitor:subsession_success(InitiatorPID, SubsessionName, InitiatorPN,
                                        InitiatorRN, InitiatorCID, Arg);
     EndType == failure ->
       actor_monitor:subsession_failure(InitiatorPID, SubsessionName, InitiatorPN,
                                        InitiatorRN, InitiatorCID, Arg)
  end,
  exit(normal),
  {noreply, State}.


handle_subsession_complete(Result, State) ->
  handle_subsession_end(success, Result, State).

handle_subsession_failed(Error, State) ->
  handle_subsession_end(failure, Error, State).

handle_get_property(Key, State) ->
  ConvProperties = State#conv_inst_state.conv_properties,
  orddict:find(Key, ConvProperties).

handle_set_property(Key, Value, State) ->
  ConvProperties = State#conv_inst_state.conv_properties,
  NewConvProperties = orddict:store(Key, Value, ConvProperties),
  State#conv_inst_state{conv_properties=NewConvProperties}.

handle_unset_property(Key, State) ->
  ConvProperties = State#conv_inst_state.conv_properties,
  NewConvProperties = orddict:erase(Key, ConvProperties),
  State#conv_inst_state{conv_properties=NewConvProperties}.

% Callbacks...
% Standard session
init([ProtocolName]) ->
  process_flag(trap_exit, true),
  {ok, fresh_state(ProtocolName)};
% Subsession
init([ProtocolName, ParentConvID, InitiatorPID, InitiatorProtocol, InitiatorRole]) ->
  process_flag(trap_exit, true),
  {ok, fresh_state(ProtocolName, ParentConvID, InitiatorPID,
                   InitiatorProtocol, InitiatorRole)}.
handle_call({get_property, Key}, _, State) ->
  Res = handle_get_property(Key, State),
  {reply, Res, State};
handle_call({set_property, Key, Value}, _, State) ->
  NewState = handle_set_property(Key, Value, State),
  {reply, ok, NewState};
handle_call({unset_property, Key}, _, State) ->
  NewState = handle_unset_property(Key, State),
  {reply, ok, NewState};
handle_call(get_root_pid, _, State) ->
  Res = handle_get_root_pid(State),
  {reply, Res, State};
handle_call(Other, Sender, State) ->
  conversation_warn("Unhandled sync message ~w from ~p", [Other, Sender], State),
  {noreply, State}.

%handle_cast({begin_continuation_safety_check, RoleName}, State) ->
%  handle_begin_continuation_safety_check(RoleName, State),
%  {noreply, State};
handle_cast({end_conversation, Reason}, State) ->
  handle_end_conversation(Reason, State);
handle_cast({start_invitations, InitiatorRole, InitiatorPID}, State) ->
  {Res, NewState} = invite_actors(InitiatorRole, InitiatorPID, State),
  {noreply, NewState};
handle_cast({start_subsession_invitations, InternalInvitations,
             ExternalInvitations, ParentRoleMapping}, State) ->
  {Res, NewState} =
    handle_send_subsession_invitations(InternalInvitations, ExternalInvitations,
                                       ParentRoleMapping, State),
  {noreply, NewState};
handle_cast({subsession_complete, Result}, State) ->
  handle_subsession_complete(Result, State);
handle_cast({subsession_failed, Reason}, State) ->
  handle_subsession_failed(Reason, State);
handle_cast(Other, State) ->
  conversation_warn("Unhandled async message ~w.", [Other], State),
  {noreply, State}.

handle_info({'DOWN', MonitorRef, _, _DownPID, _Reason}, State) ->
  handle_begin_continuation_safety_check(MonitorRef, State),
  {noreply, State};
handle_info(Msg, State) ->
  conversation_warn("Unhandled Info message ~w.", [Msg], State),
  {noreply, State}.

code_change(_Prev, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%%%
%%%% API
%%%%

start_link(ProtocolName) ->
  gen_server2:start_link(conversation_instance, [ProtocolName], []).

end_conversation(ConvID, Reason) ->
  gen_server2:cast(ConvID, {end_conversation, Reason}).

start_link(ProtocolName, ParentConvID, InitiatorPID,
           InitiatorProtocol, InitiatorRole) ->
  gen_server2:start_link(conversation_instance, [ProtocolName, ParentConvID,
                                            InitiatorPID, InitiatorProtocol,
                                            InitiatorRole], []).

start_invitations(ConvID, InitiatorRole, InitiatorPID) ->
  gen_server2:cast(ConvID, {start_invitations, InitiatorRole, InitiatorPID}).

start_subsession_invitations(SubsessionID, InternalInvitations, ExternalInvitations, ParentRoleMapping) ->
  gen_server2:cast(SubsessionID, {start_subsession_invitations, InternalInvitations,
                                  ExternalInvitations, ParentRoleMapping}).

get_property(ConvID, Key) ->
  gen_server2:call(ConvID, {get_property, Key}).

set_property(ConvID, Key, Value) ->
  gen_server2:call(ConvID, {set_property, Key, Value}).

unset_property(ConvID, Key) ->
  gen_server2:call(ConvID, {unset_property, Key}).

get_root_pid(ConvID) ->
  gen_server2:call(ConvID, get_root_pid).

subsession_complete(ConvID, Result) ->
  gen_server2:cast(ConvID, {subsession_complete, Result}).

subsession_failed(ConvID, Reason) ->
  gen_server2:cast(ConvID, {subsession_failed, Reason}).

