-module(conversation_instance).
-compile(export_all).
-behaviour(gen_server2).

-record(subsession_state, {initiator_pid,
                           initiator_protocol,
                           initiator_role,
                           parent_conv_id}).

-record(conv_inst_state, {protocol_name,             %% Name of the protocol

                          role_mapping,              %% Role |-> Endpoint mapping

                          setup_complete_broadcast,  %% Flag: set to true when
                                                     %% setup success msg sent

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
% ConvID: PID of conversation
% RoleName: Role we're monitoring
% Endpoint: PID of role inhabitant
erlang_monitor_participant(RoleName, Endpoint, State) ->
  MonitorRef = erlang:monitor(process, Endpoint),
  ParticipantMonitors = State#conv_inst_state.participant_monitor_refs,
  NewParticipantMonitors = orddict:store(MonitorRef,
                                         RoleName, ParticipantMonitors),
  State#conv_inst_state{participant_monitor_refs=NewParticipantMonitors}.

% Checks whether the conversation is complete, and sends out
% "complete" messages if so
check_conversation_setup_complete(State) ->
  RoleMapping = State#conv_inst_state.role_mapping,
  AlreadySetup = State#conv_inst_state.setup_complete_broadcast,
  SetupComplete = orddict:fold(
                    fun(_K, V, A) ->
                        Res = V =/= not_filled,
                        Res and A end, true, RoleMapping),
  % If it is complete, broadcast the conv setup complete message
  if not AlreadySetup andalso SetupComplete ->
      broadcast_conv_setup(State),
      % notify_subsession_established(State),
      State#conv_inst_state{setup_complete_broadcast=true};
    true -> State
  end.


notify_subsession_established(State) ->
  SubsessionState = State#conv_inst_state.subsession_state,
  if SubsessionState =/= undefined ->
       InitiatorPID = SubsessionState#subsession_state.initiator_pid,
       InitiatorRole = SubsessionState#subsession_state.initiator_role,
       ParentConvID = SubsessionState#subsession_state.parent_conv_id,
       actor_monitor:subsession_established(InitiatorPID, ParentConvID, InitiatorRole, self());
     true ->
       ok
  end.



broadcast_conv_setup(State) ->
  ProtocolName = State#conv_inst_state.protocol_name,
  RoleMapping = State#conv_inst_state.role_mapping,
  orddict:fold(fun(K, V, _A) ->
                   if is_pid(V) ->
                        actor_monitor:conversation_success(V, ProtocolName, K, self(), RoleMapping);
                      true -> ok
                   end end, ok, RoleMapping).


% Add the participant to the Role |-> Endpoint map
register_participant(RoleName, Sender, State) ->
  RoleMap = State#conv_inst_state.role_mapping,
  IsKey = orddict:is_key(RoleName, RoleMap),
  NewRoleMap = if IsKey ->
                    orddict:store(RoleName, Sender, RoleMap);
                  not IsKey ->
                    conversation_warn("Tried to register non-member role ~s",
                                      [RoleName], State),
                    RoleMap
               end,
  NewState = State#conv_inst_state{role_mapping=NewRoleMap},
  % Now check whether all non-transient roles have been
  % fulfilled, notifying actors if so
  NewState1 = check_conversation_setup_complete(NewState),
  % TODO: Only do this if push-based
  %NewState2 = erlang_monitor_participant(RoleName, Sender, NewState1),
  {noreply, NewState1}.

% Checks whether the role is transient in the rolespec or not.
% Initial val is not_filled if it isn't, and not_filled_transient if it is.
initial_filled_val({RoleName, {local_protocol, _, _, _, Roles, _}}) ->
  lists:foldl(fun({Ty, RN}, Acc) ->
                  if RN == RoleName andalso Ty == transient_role_decl ->
                       not_filled_transient;
                     true -> Acc
                  end end, not_filled, Roles).


fresh_state(ProtocolName, RoleSpecs) ->
% Add the names to the map, so we can ensure we accept only roles which are
  % meant to be accepted...
  EmptyMap = orddict:from_list(lists:map(fun({RN, RS}) ->
                                             {RN, initial_filled_val({RN, RS})}
                                             end, RoleSpecs)),
  #conv_inst_state{protocol_name=ProtocolName, role_mapping=EmptyMap,
                   setup_complete_broadcast=false, participant_monitor_refs=orddict:new(),
                   conv_properties=orddict:new(),
                   subsession_state=undefined
                  }.

fresh_state(ProtocolName, RoleSpecs, ParentConvID, InitiatorPID, InitiatorProtocol, InitiatorRole) ->
  State = fresh_state(ProtocolName, RoleSpecs),
  SubsessionState = #subsession_state{initiator_pid=InitiatorPID,
                                      initiator_protocol=InitiatorProtocol,
                                      initiator_role=InitiatorRole,
                                      parent_conv_id=ParentConvID},
  State#conv_inst_state{subsession_state=SubsessionState}.

broadcast_end_notification(Reason, State) ->
  % Only send one notification per actor.
  RoleMappingList = orddict:to_list(State#conv_inst_state.role_mapping),
  PIDs = lists:map(fun({_, Pid}) -> Pid end, RoleMappingList),
  UniqList = sets:to_list(sets:from_list(PIDs)),
  lists:foreach(fun(Pid) -> actor_monitor:conversation_ended(Pid, self(), Reason) end,
                UniqList).

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

handle_begin_continuation_safety_check(MonitorRef, State) ->
  RoleMapping = State#conv_inst_state.role_mapping,
  ParticipantMonitorRefs = State#conv_inst_state.participant_monitor_refs,
  case orddict:find(MonitorRef, ParticipantMonitorRefs) of
    {ok, RoleName} ->
      % Iterate over each -- should a node fail, ignore it, it'll be picked up later
      UsedInRemainder =
        orddict:fold(fun(LocalRole, Endpoint, Acc) ->
                       Res = case (catch (actor_monitor:check_role_reachable(Endpoint, self(),
                                                                 LocalRole, RoleName))) of
                         true -> true;
                         false -> false;
                         _Other -> false
                       end,
                       Res or Acc end, false, RoleMapping),
      % Now, if Res is false, then the dead role's not reachable from anywhere.
      % In which case, celebrate, we can continue as normal.
      % If not, we'll need to do some stuff. For now, we can end the session.
      if UsedInRemainder ->
           end_conversation(self(), {role_offline, RoleName});
         not UsedInRemainder -> ok
      end;
    error -> ok
  end.

handle_get_endpoints(RoleList, State) ->
  RoleMapping = State#conv_inst_state.role_mapping,
  lists:map(fun(Role) -> {Role, orddict:fetch(Role, RoleMapping)} end, RoleList).

% InternalRoles : [Role]
% ExternalInvitations : [{Role, Endpoint}]
handle_send_subsession_invitations(InternalRoles, ExternalInvitations, State) ->
  % Load everything we need from the state
  SubsessionState = State#conv_inst_state.subsession_state,
  InitiatorPID = SubsessionState#subsession_state.initiator_pid,
  InitiatorProtocol = SubsessionState#subsession_state.initiator_protocol,
  InitiatorRole = SubsessionState#subsession_state.initiator_role,
  ParentConvID = SubsessionState#subsession_state.parent_conv_id,
  ProtocolName = State#conv_inst_state.protocol_name,

  % Get the inhabitants of all internally-invited roles
  InternalInvitations = conversation_instance:get_endpoints(ParentConvID, InternalRoles),

  % InternalInvitations, ExternalInvitations: [{Role, Endpoint}]
  InvitationList = InternalInvitations ++ ExternalInvitations,
  % Invite everything in the invitation list
  FailList = lists:foldr(fun({Role, Endpoint}, FailList) ->
                    try actor_monitor:incoming_invitation(Endpoint,
                                                          ProtocolName,
                                                          Role,
                                                          self()) of
                      ok ->
                        % We're golden, it went through, and we'll get the response
                        % soon.
                        FailList
                    catch
                      _:Err ->
                        error_logger:error_msg("Subsession invitation of role ~p failed: ~p~n",
                                               [Role, Err]),
                        [Role|FailList]
                    end
              end, [], InvitationList),
  if length(FailList) == 0 ->
       ok;
     length(FailList) =/= 0 ->
       actor_monitor:subsession_setup_failed(InitiatorPID, ProtocolName, self(),
                                             InitiatorProtocol, InitiatorRole,
                                             ParentConvID, invitation_failed),
       end_conversation(self(), invitation_failed)
  end.

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
       error_logger:info_msg("IN HSE CINST. GOD FUCKING HELP ME.~n SubsessionName: ~p~nSubsessionPID: ~p~nInitiatorPN: ~p~n
                        InitiatorRN: ~p~n, InitiatorCID: ~p~n, Result: ~p~n",
                        [SubsessionName, InitiatorPID, InitiatorPN, InitiatorRN, InitiatorCID, Arg]),
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
init([ProtocolName, RoleSpecs]) ->
  process_flag(trap_exit, true),
  {ok, fresh_state(ProtocolName, RoleSpecs)};
% Subsession
init([ProtocolName, RoleSpecs, ParentConvID, InitiatorPID, InitiatorProtocol, InitiatorRole]) ->
  process_flag(trap_exit, true),
  {ok, fresh_state(ProtocolName, RoleSpecs, ParentConvID, InitiatorPID,
                   InitiatorProtocol, InitiatorRole)}.

handle_call({get_endpoints, RoleList}, _, State) ->
  Res = handle_get_endpoints(RoleList, State),
  {reply, Res, State};
handle_call({get_property, Key}, _, State) ->
  Res = handle_get_property(Key, State),
  {reply, Res, State};
handle_call({set_property, Key, Value}, _, State) ->
  NewState = handle_set_property(Key, Value, State),
  {reply, ok, NewState};
handle_call({unset_property, Key}, _, State) ->
  NewState = handle_unset_property(Key, State),
  {reply, ok, NewState};

handle_call(Other, Sender, State) ->
  conversation_warn("Unhandled sync message ~w from ~p", [Other, Sender], State),
  {noreply, State}.

%handle_cast({begin_continuation_safety_check, RoleName}, State) ->
%  handle_begin_continuation_safety_check(RoleName, State),
%  {noreply, State};
handle_cast({end_conversation, Reason}, State) ->
  handle_end_conversation(Reason, State);
handle_cast({accept_invitation, RoleName, Sender}, State) ->
  register_participant(RoleName, Sender, State);
handle_cast({start_subsession_invitations, InternalInvitations,
             ExternalInvitations}, State) ->
  handle_send_subsession_invitations(InternalInvitations, ExternalInvitations, State),
  {noreply, State};
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

start(ProtocolName, RoleSpecs) ->
  gen_server2:start(conversation_instance, [ProtocolName, RoleSpecs], []).

start(ProtocolName, RoleSpecs, FailureHandlerFn) ->
  gen_server2:start(conversation_instance, [ProtocolName, RoleSpecs, FailureHandlerFn], []).

end_conversation(ConvID, Reason) ->
  gen_server2:cast(ConvID, {end_conversation, Reason}).

start_subsession(ProtocolName, RoleSpecs, ParentConvID, InitiatorPID, InitiatorProtocol, InitiatorRole) ->
  gen_server2:start(conversation_instance, [ProtocolName, RoleSpecs, ParentConvID,
                                            InitiatorPID, InitiatorProtocol, InitiatorRole], []).


start_subsession_invitations(SubsessionID, InternalInvitations, ExternalInvitations) ->
  gen_server2:cast(SubsessionID, {start_subsession_invitations, InternalInvitations,
                                  ExternalInvitations}).

accept_invitation(ConversationID, RoleName, ProcessID) ->
  gen_server2:cast(ConversationID, {accept_invitation, RoleName, ProcessID}).


get_endpoints(ConvID, RoleList) ->
  gen_server2:call(ConvID, {get_endpoints, RoleList}).

get_property(ConvID, Key) ->
  gen_server2:call(ConvID, {get_property, Key}).

set_property(ConvID, Key, Value) ->
  gen_server2:call(ConvID, {set_property, Key, Value}).

unset_property(ConvID, Key) ->
  gen_server2:call(ConvID, {unset_property, Key}).

subsession_complete(ConvID, Result) ->
  gen_server2:cast(ConvID, {subsession_complete, Result}).

subsession_failed(ConvID, Reason) ->
  gen_server2:cast(ConvID, {subsession_failed, Reason}).

