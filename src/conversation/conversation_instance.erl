-module(conversation_instance).
-compile(export_all).
-behaviour(gen_server2).

-record(conv_inst_state, {protocol_name, % Name of the protocol
                          role_states, % State of each role (filled, not_filled, not_filled_transient)
                          role_monitor_mapping, % Role |-> Monitor process mapping
                          setup_complete_broadcast % Set when setup_complete has been broadcast
                         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Logging Functions
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



% Message routing.
% Get and test endpoints for all roles.
% Returns either {ok, [Endpoints]} if all's ok,
% {error, role_not found, Role} if role is not in the mapping table,
% {error, endpoint_terminated, Role} if the Role |-> Endpoint mapping
% is stale
is_a_node(Node) ->
  Node =/= nonode@nohost.


% TODO: This will need to change in order to properly implement failure
% handling, as the architecture has changed since this was written.
get_endpoints([], _RoleMap) -> {ok, []};
get_endpoints([Role|Roles], RoleMap) ->
  case orddict:find(Role, RoleMap) of
    {ok, Endpoint} ->
      RemoteAlive = (is_a_node(node()) and is_a_node(node(Endpoint))),
      LocalAlive = (catch is_process_alive(Endpoint)),
      ProcessAlive = (RemoteAlive or (LocalAlive == true)),
      if ProcessAlive ->
           case get_endpoints(Roles, RoleMap) of
             {ok, Endpoints} ->
               {ok, [{Role, Endpoint}|Endpoints]};
             Err -> Err
           end;
         not ProcessAlive ->
           {error, endpoint_terminated, Role}
      end;
    error ->
      {error, role_not_found, Role}
  end.


%%% Handles an outgoing message, by monitoring and sending to the conversation
%%% instance for routing if all is well.
handle_outgoing_message(RoleName, Recipients, MessageName, Types, Payload, State) ->
  % Construct a message instance, send to the monitor, and check the result
  Message = message:message(make_ref(), RoleName, Recipients,
                            MessageName, Types, Payload),
  Monitors = State#conv_inst_state.role_monitor_mapping,
  MonitorPID = orddict:fetch(RoleName, Monitors),
  MonitorRes = role_monitor:send_message(MonitorPID, Message),
  case MonitorRes of
      ok -> route_message(Message, State);
      Err -> {reply, Err, State}
  end.


% Lookup the destination role, and forward to its monitor.
route_message(Msg, State) ->
  io:format("Message data: ~p~n", [Msg]),
  Recipients = message:message_recipients(Msg),
  RoleMap = State#conv_inst_state.role_monitor_mapping,
  % Lookup the endpoint for each recipient and deliver
  case get_endpoints(Recipients, RoleMap) of
    {ok, Endpoints} ->
      lists:foreach(fun({_DestRole, Endpoint}) ->
                        role_monitor:receive_message(Endpoint, Msg),
                        ok
                    end, Endpoints),
      {reply, ok, State};
    Err -> {reply, Err, State}
  end.

% Checks whether the conversation is complete, and sends out
% "complete" messages if so
check_conversation_setup_complete(State) ->
  RoleMapping = State#conv_inst_state.role_states,
  AlreadySetup = State#conv_inst_state.setup_complete_broadcast,
  SetupComplete = orddict:fold(
                    fun(_K, V, A) ->
                        Res = V =/= not_filled,
                        Res and A end, true, RoleMapping),
  % If it is complete, broadcast the conv setup complete message
  io:format("Setup complete? ~p; Already setup? ~p; RoleMapping: ~p~n",
            [SetupComplete, AlreadySetup, RoleMapping]),
  if not AlreadySetup andalso SetupComplete ->
      broadcast_conv_setup(State),
      State#conv_inst_state{setup_complete_broadcast=true};
    true -> State
  end.

broadcast_conv_setup(State) ->
  RoleMapping = State#conv_inst_state.role_monitor_mapping,
  orddict:fold(fun(_K, V, _A) ->
                   if is_pid(V) ->
                        role_monitor:conversation_success(V, self());
                      true -> ok
                   end end, ok, RoleMapping),
  ok.


set_role_actor_pid(Role, ActorPID, RoleMonitorMap) ->
  MonitorPID = orddict:fetch(Role, RoleMonitorMap),
  role_monitor:set_actor_pid(MonitorPID, ActorPID).

% Add the participant to the Role |-> Endpoint map
register_participant(RoleName, Sender, State) ->
  RoleStatesMap = State#conv_inst_state.role_states,
  RoleMonitorMap = State#conv_inst_state.role_monitor_mapping,
  IsKey = orddict:is_key(RoleName, RoleStatesMap),
  NewRoleMap = if IsKey ->
                    set_role_actor_pid(RoleName, Sender, RoleMonitorMap),
                    orddict:store(RoleName, Sender, RoleStatesMap);
                  not IsKey ->
                    conversation_warn("Tried to register non-member role ~s",
                                      [RoleName], State),
                    RoleStatesMap
               end,
  NewState = State#conv_inst_state{role_states=NewRoleMap},
  % Now check whether all non-transient roles have been
  % fulfilled, notifying actors if so
  NewState1 = check_conversation_setup_complete(NewState),
  {reply, ok, NewState1}.

% Checks whether the role is transient in the rolespec or not.
% Initial val is not_filled if it isn't, and not_filled_transient if it is.
initial_filled_val({RoleName, {local_protocol, _, _, _, Roles, _}}) ->
  lists:foldl(fun({Ty, RN}, Acc) ->
                  if RN == RoleName andalso Ty == transient_role_decl ->
                       not_filled_transient;
                     true -> Acc
                  end end, not_filled, Roles).


fresh_state(ProtocolName, RoleNames) ->
  % Add the names to the map, so we can ensure we accept only roles which are
  % meant to be accepted...
  EmptyMap = orddict:from_list(lists:map(fun({RN, RS}) ->
                                             {RN, initial_filled_val({RN, RS})}
                                             end, RoleNames)),
  #conv_inst_state{protocol_name=ProtocolName, role_states=EmptyMap,
                   setup_complete_broadcast=false}.

handle_end_conversation(Reason, State) ->
  % Only send one notification per actor.
  % I think, for now, at least.
  RoleMappingList = orddict:to_list(State#conv_inst_state.role_monitor_mapping),
  PIDs = lists:map(fun({_, Pid}) -> Pid end, RoleMappingList),
  UniqList = sets:to_list(sets:from_list(PIDs)),
  lists:foreach(fun(Pid) -> role_monitor:conversation_ended(Pid, Reason) end,
                UniqList),
  exit(normal),
  {noreply, State}.


spawn_monitors_inner([], _PN, State, MonitorDict) ->
  {true, State#conv_inst_state{role_monitor_mapping=MonitorDict}};
spawn_monitors_inner([{RoleName, Monitor}|Ms], ProtocolName, State, MonitorDict) ->
 % io:format("Wtf is M, then? ~p~n", [M]),
  SpawnRes = role_monitor:start_link(ProtocolName, RoleName, self(), Monitor),
  case SpawnRes of
    {ok, Pid} ->
      NewMonitors = orddict:store(RoleName, Pid, MonitorDict),
      spawn_monitors_inner(Ms, ProtocolName, State, NewMonitors);
    Err -> error_logger:error_msg("Error starting monitor process: ~p~n", Err),
           false
  end.


spawn_monitor_processes(State, Monitors) ->
  ProtocolName = State#conv_inst_state.protocol_name,
  SpawnRes = spawn_monitors_inner(Monitors, ProtocolName, State, orddict:new()),
  case SpawnRes of
    {true, NewState} -> {ok, NewState};
    false -> {error, error_starting_monitors}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ProtocolName, RoleSpecs, Monitors]) ->
  State = fresh_state(ProtocolName, RoleSpecs),
  spawn_monitor_processes(State, Monitors).

handle_call({accept_invitation, RoleName}, {Sender, _}, State) ->
  register_participant(RoleName, Sender, State);
handle_call({outgoing_msg, RoleName, Recipients, MessageName, Types, Payload},
            _From, State) ->
  handle_outgoing_message(RoleName, Recipients, MessageName, Types, Payload, State);
handle_call(Other, Sender, State) ->
  conversation_warn("Unhandled sync message ~w from ~p", [Other, Sender], State),
  {noreply, State}.

handle_cast({end_conversation, Reason}, State) ->
  handle_end_conversation(Reason, State);
handle_cast(Other, State) ->
  conversation_warn("Unhandled async message ~w.", [Other], State),
  {noreply, State}.


handle_info(Msg, State) ->
  conversation_warn("Unhandled Info message ~w.", [Msg], State),
  {noreply, State}.

code_change(_Prev, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(ProtocolName, Roles, Monitors) ->
  gen_server2:start(conversation_instance, [ProtocolName, Roles, Monitors], []).

outgoing_message(Role, ConversationID, Recipients, MessageName, Types, Payload) ->
  gen_server:call(ConversationID, {outgoing_msg, Role, Recipients, MessageName, Types, Payload}).

end_conversation(ConvID, Reason) ->
  gen_server2:cast(ConvID, {end_conversation, Reason}).

