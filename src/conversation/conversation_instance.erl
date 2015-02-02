-module(conversation_instance).
-compile(export_all).
-behaviour(gen_server).

-record(conv_inst_state, {protocol_name,
                          role_mapping}).

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
  log_msg(fun error_logger:warn_message/2, Format, Args, State).

conversation_error(Format, Args, State) ->
  log_msg(fun error_logger:error_message/2, Format, Args, State).

conversation_info(Format, Args, State) ->
  log_msg(fun error_logger:info_message/2, Format, Args, State).


% Message routing.
% Lookup the destination role, and forward to its monitor.
route_message(Msg, State) ->
  Recipients = message:message_recipients(Msg),
  RoleMap = State#conv_inst_state.role_mapping,
  % Lookup the endpoint for each recipient and deliver
  lists:iter(fun (Recipient) ->
                 case orddict:find(Recipient, RoleMap) of
                  {ok, Endpoint} ->
                    gen_server:cast(Endpoint, {message, Msg});
                  error ->
                    conversation_warn("Couldn't find endpoint for role ~s.",
                                      [Recipient], State)
                 end end, Recipients).

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
  {reply, ok, State#conv_inst_state{role_mapping=NewRoleMap}}.

fresh_state(ProtocolName, RoleNames) ->
  % Add the names to the map, so we can ensure we accept only roles which are
  % meant to be accepted...
  EmptyMap = orddict:from_list(lists:map(fun(RoleName) ->
                                             {RoleName, undefined} end,
                                        RoleNames)),
  #conv_inst_state{protocol_name=ProtocolName, role_mapping=EmptyMap}.

% Callbacks...
init([ProtocolName, RoleNames]) -> {ok, fresh_state(ProtocolName, RoleNames)}.

handle_call({accept_invitation, RoleName}, Sender, State) ->
  register_participant(RoleName, Sender, State);
handle_call(Other, Sender, State) ->
  conversation_warn("Unhandled sync message ~w from ~p", [Other, Sender], State),
  {noreply, State}.

handle_cast({outgoing_msg, Msg}, State) ->
  route_message(Msg, State),
  {noreply, State};
handle_cast(Other, State) ->
  conversation_warn("Unhandled async message ~w.", [Other], State),
  {noreply, State}.


handle_info(Msg, State) ->
  conversation_warn("Unhandled Info message ~w.", [Msg], State),
  {noreply, State}.

code_change(_Prev, State, _Extra) -> {ok, State}.
terminate(Reason, State) -> ok.
