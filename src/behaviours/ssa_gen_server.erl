-module(ssa_gen_server).
-behaviour(gen_server2).

-compile(export_all).
-record(actor_state, {actor_type_name,
                      proxy_pid,
                      user_state}).

% This is the behaviour for basic session actors.
% The behaviour requires two callbacks:
%  * ssactor_init, which returns the initial user state
%  * ssactor_handle_msg, which handles incoming messages.
%
% The session actor itself has certain bits of internal state, which we
% leverage in order to perform proxying and message routing. This is:
%  * Conversation process PID: We use this in order to relay messages to
%    the conversation process, which performs Role |-> Endpoint routing.
%
%  * Roles the actor is playing in the current conversation
%
%  * The currently-active role in the actor
%
%  * And finally, a Role |-> proxy mapping.


% ssactor_init returns some state given some input args
% ssactor_handle_message handles a message.
%   Return values:
%    {ok, NewUserState}
%    {ok, NewUserState, NewRoleState}
%    {stop, Reason, NewUserState}
%    {stop, Reason, NewUserState, NewRoleState}
%
% ssactor_join is called when the actor is invited to participate in a
%   conversation. The user can decide to accept or decline this invitation.
% ssactor_conversation_established is called when all actors have been invited.
% ssactor_conversation_error is called when there was an error establishing the
%   conversation.
behaviour_info(callbacks) ->
    [{ssactor_init,2},
     {ssactor_join,4},
     {ssactor_handle_message, 7},
     {ssactor_become, 5},
     {ssactor_join, 4},
     {ssactor_conversation_established, 5},
     {ssactor_conversation_error, 4},
     {ssactor_conversation_ended, 3},
     {handle_call, 3},
     {handle_cast, 3},
     {handle_info, 3},
     {terminate, 2}
    ];
behaviour_info(_Other) ->
    undefined.

update_user_state(SystemState, NewUserState) ->
  SystemState#actor_state{user_state = NewUserState}.

log_msg(Func, Format, Args, State) ->
  InfoStr = "SSACTOR: Actor ~p, actor PID ~p, proxy PID ~p.",
  InfoArgs = [State#actor_state.actor_type_name,
              self(),
              State#actor_state.proxy_pid],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

actor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

actor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

actor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).


% gen_server2 callbacks
init([Module, UserArgs]) ->
  ProtocolRoleMap = actor_type_registry:get_protocol_role_map(Module),
  ProxyProcess = actor_proxy:start_link(self(), Module, ProtocolRoleMap),
  finish_init(Module, UserArgs, ProxyProcess);
init([RegName, Module, UserArgs]) ->
  ProtocolRoleMap = actor_type_registry:get_protocol_role_map(Module),
  ProxyProcess = actor_proxy:start_link(RegName, self(), Module, ProtocolRoleMap),
  finish_init(Module, UserArgs, ProxyProcess).


finish_init(Module, UserArgs, ProxyProcess) ->
  process_flag(trap_exit, true),
  case ProxyProcess of
    {ok, ProxyPID} ->
      actor_type_registry:register_actor_instance(Module, ProxyPID),
      UserState = Module:ssactor_init(UserArgs, ProxyPID),
      {ok, #actor_state{actor_type_name=Module,
                        proxy_pid=ProxyPID,
                        user_state=UserState}};
    Other ->
      {error, {proxy_start_failed, Other}}
  end.

% Delegate calls, casts (other than ssa internal messages), info messages
% and termination messages to the actor.

delegate_async(Fun, Msg, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  UserResult = case Fun of
                 handle_cast ->
                   Module:handle_cast(Msg, UserState);
                 handle_info ->
                   Module:handle_info(Msg, UserState)
               end,
  % Propagate user state changes without losing system state
  case UserResult of
    {noreply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState};
    {noreply, NewUserState, Arg} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState, Arg};
    {stop, Reason, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {stop, Reason, NewState}
  end.

% Same for synchronous messages, but there are a couple more things we need
% to handle, in particular re: replies
handle_call(ssa_get_proxy_id, _From, State) ->
  {reply, State#actor_state.proxy_pid, State};
handle_call({ssa_join_conversation, ProtocolName, RoleName, ConversationID},
            _From, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  UserResult = Module:ssactor_join(ProtocolName, RoleName, ConversationID, UserState),
  case UserResult of
    {accept, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {reply, accept, NewState};
    {decline, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {reply, decline, NewState}
  end;
handle_call({delegate_call, From, Msg}, _From, State) ->
  % Spoof From value
  handle_call(Msg, From, State);
handle_call(Request, From, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  UserResult = Module:handle_call(Request, From, UserState),
  case UserResult of
    {reply, Reply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {reply, Reply, NewState};
    {reply, Reply, NewUserState, Arg} ->
      NewState = update_user_state(State, NewUserState),
      {reply, Reply, NewState, Arg};
    {noreply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState};
    {noreply, NewUserState, Arg} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState, Arg};
    {stop, Reason, Reply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {stop, Reason, Reply, NewState};
    {stop, Reason, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {stop, Reason, NewState}
  end.

% Handle incoming user messages. These have been checked by the proxy to
% ensure that they conform to the MPST.
handle_cast({ssa_msg, MonitorPID, Protocol, Role, ConversationID, MsgData}, State) ->
                   % {message, _, Sender, _, Op, Types, Payload}}, State) ->
  actor_info("Processing message ~p", [MsgData], State),
  Sender = message:message_sender(MsgData),
  Op = message:message_name(MsgData),
  Payload = message:message_payload(MsgData),
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  % Notify monitor that we've started processing the message
  role_monitor:handler_started(MonitorPID),
  HandleRes = Module:ssactor_handle_message(
                   Protocol, Role, ConversationID, Sender, Op, Payload, UserState,
                   {Protocol, Role, ConversationID, State#actor_state.proxy_pid}),
  % Save role state, reset handler state to idle, and grab new user state
  NewUserState =
    case HandleRes of
      {ok, NewState} ->
        role_monitor:handler_finished(MonitorPID),
        NewState;
      {ok, NewState, NewRoleState} ->
        role_monitor:handler_finished(MonitorPID, NewRoleState),
        NewState;
      {stop, NewState} ->
        % TODO: yadayada need to do some stuff to stop here
        role_monitor:handler_finished(MonitorPID),
        NewState;
      {stop, NewState, NewRoleState} ->
        % As above
        role_monitor:handler_finished(MonitorPID, NewRoleState),
        NewState;
      Other ->
        exit(bad_return_value)
    end,
  {noreply, State#actor_state{user_state=NewUserState}};

% Become
handle_cast(_Msg = {become, Protocol, Role, Operation, Arguments, CID}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  NewUserState =
    Module:ssactor_become(Protocol, Role, Operation, Arguments,
                          {Protocol, Role, CID, State#actor_state.proxy_pid},
                          UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
% Setup failed
handle_cast(_Msg = {ssa_conversation_setup_failed, Protocol, Role, Err}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  {ok, NewUserState} = Module:ssactor_conversation_error(Protocol, Role, Err, UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
% Setup successful
handle_cast(_Msg = {ssa_session_established, Protocol, Role, CID}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  ConvKey = {Protocol, Role, CID, State#actor_state.proxy_pid},
  {ok, NewUserState} =
    Module:ssactor_conversation_established(Protocol, Role, CID,
                                            ConvKey, UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
% Conversation ended
handle_cast({conversation_ended, CID, Reason}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  {ok, NewUserState} = Module:ssactor_conversation_ended(CID, Reason, UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
handle_cast(Msg, State) ->
  delegate_async(handle_cast, Msg, State).


% Info messages -- we don't do anything with these
handle_info(Msg, State) ->
  delegate_async(handle_info, Msg, State).

% We don't need this.
code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  actor_error("Actor terminating for reason ~p~n", [Reason], State),
  Module = State#actor_state.actor_type_name,
  ProxyPID = State#actor_state.proxy_pid,
  UserState = State#actor_state.user_state,
  actor_type_registry:deregister_actor_instance(Module, ProxyPID),
  Module:terminate(Reason, UserState),
  ok.


% Gets the proxy PID to return to user code, as the user should
% address all requests (even unmonitored ones) to the proxy, which
% forwards the requests.
unwrap_start_result({ok, ActorPid}) ->
  ProxyPID = gen_server2:call(ActorPid, ssa_get_proxy_id),
  {ok, ProxyPID};
unwrap_start_result(Other) -> Other.


%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal API %%%%
%%%%%%%%%%%%%%%%%%%%%%
conversation_ended(ActorPID, CID, Reason) ->
  gen_server2:cast(ActorPID, {conversation_ended, CID, Reason}).

message(ActorPID, MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:cast(ActorPID, {ssa_msg, MonitorPID, ProtocolName, RoleName, ConvID, Msg}).


%%%%%%%%%%%%%%%%%%%%%%
%%%%  Public  API %%%%
%%%%%%%%%%%%%%%%%%%%%%

start(ModuleName, Args, Options) ->
  Res = gen_server2:start(ssa_gen_server, [ModuleName, Args], Options),
  unwrap_start_result(Res).

start(RegName, ModuleName, Args, Options) ->
  Res = gen_server2:start(ssa_gen_server, [RegName, ModuleName, Args], Options),
  unwrap_start_result(Res).

start_link(ModuleName, Args, Options) ->
  Res = gen_server2:start_link(ssa_gen_server, [ModuleName, Args], Options),
  unwrap_start_result(Res).

start_link(RegName, ModuleName, Args, Options) ->
  Res = gen_server2:start_link(ssa_gen_server, [RegName, ModuleName, Args], Options),
  unwrap_start_result(Res).


call(ServerRef, Message) ->
  gen_server2:call(ServerRef, Message).

call(ServerRef, Message, Timeout) ->
  gen_server2:call(ServerRef, Message, Timeout).

cast(ServerRef, Message) ->
  gen_server2:cast(ServerRef, Message).

reply(ServerRef, Message) ->
  gen_server2:reply(ServerRef, Message).

