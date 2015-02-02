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

% Callbacks...
init(Args) -> {ok, {}}.

handle_call(Msg, Sender, State) -> {noreply, State}.

handle_cast({outgoing_msg, Msg}, State) ->
  route_message(Msg, State),
  {noreply, State};
handle_cast(Other, State) ->
  conversation_warn("Unhandled async message ~w.", [Other], State),
  {noreply, State}.


handle_info(Msg, State) -> {noreply, State}.

code_change(_Prev, State, _Extra) -> {ok, State}.
terminate(Reason, State) -> ok.
