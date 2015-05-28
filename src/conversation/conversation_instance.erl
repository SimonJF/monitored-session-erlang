-module(conversation_instance).
-compile(export_all).
-behaviour(gen_server2).

-record(conv_inst_state, {protocol_name,
                          role_mapping,
                          setup_complete_broadcast
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
      State#conv_inst_state{setup_complete_broadcast=true};
    true -> State
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
  % TODO: Possibly take more drastic action? Or just ignore...
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
  #conv_inst_state{protocol_name=ProtocolName, role_mapping=EmptyMap,
                   setup_complete_broadcast=false}.

handle_end_conversation(Reason, State) ->
  % Only send one notification per actor.
  % I think, for now, at least.
  RoleMappingList = orddict:to_list(State#conv_inst_state.role_mapping),
  PIDs = lists:map(fun({_, Pid}) -> Pid end, RoleMappingList),
  UniqList = sets:to_list(sets:from_list(PIDs)),
  lists:foreach(fun(Pid) -> actor_monitor:conversation_ended(Pid, self(), Reason) end,
                UniqList),
  exit(normal),
  {noreply, State}.



% Callbacks...
init([ProtocolName, RoleSpecs]) -> {ok, fresh_state(ProtocolName, RoleSpecs)}.

handle_call({accept_invitation, RoleName}, {Sender, _}, State) ->
  register_participant(RoleName, Sender, State);
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

%%%%
%%%% API
%%%%

start(ProtocolName, Roles) ->
  gen_server2:start(conversation_instance, [ProtocolName, Roles], []).

end_conversation(ConvID, Reason) ->
  gen_server2:cast(ConvID, {end_conversation, Reason}).

