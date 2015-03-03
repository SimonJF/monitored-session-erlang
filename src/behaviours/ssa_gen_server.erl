-module(ssa_gen_server).
-behaviour(gen_server).

-compile(export_all).
-record(actor_state, {actor_type_name,
                      monitor_pid,
                      user_state}).

% This is the behaviour for basic session actors.
% The behaviour requires two callbacks:
%  * ssactor_init, which returns the initial user state
%  * ssactor_handle_msg, which handles incoming messages.
%
% The session actor itself has certain bits of internal state, which we
% leverage in order to perform monitoring and message routing. This is:
%  * Conversation process PID: We use this in order to relay messages to
%    the conversation process, which performs Role |-> Endpoint routing.
%
%  * Roles the actor is playing in the current conversation
%
%  * The currently-active role in the actor
%
%  * And finally, a Role |-> Monitor mapping.


% ssactor_init returns some state given some input args
% ssactor_handle_message:
%   SenderRole ->
%   OperatorName ->
%   PayloadTypes(?) ->
%   Payload List ->
%   State ->
%   NewState
behaviour_info(callbacks) ->
    [{ssactor_init,2},
     {ssactor_handle_message, 6},
     {ssactor_become, 5},
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
  InfoStr = "SSACTOR: Actor ~p, actor PID ~p, monitor PID ~p.",
  InfoArgs = [State#actor_state.actor_type_name,
              self(),
              State#actor_state.monitor_pid],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

actor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

actor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

actor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).


% gen_server callbacks
init([Module, UserArgs]) ->
  ProtocolRoleMap = actor_type_registry:get_protocol_role_map(Module),
  MonitorProcess = gen_server:start_link(actor_monitor,
                                         [self(),
                                          Module,
                                          ProtocolRoleMap], []),
  case MonitorProcess of
    {ok, MonitorPid} ->
      actor_type_registry:register_actor_instance(Module, MonitorPid),
      % TODO: Must send the protocol and role here too
      UserState = Module:ssactor_init(UserArgs, MonitorPid),
      {ok, #actor_state{actor_type_name=Module,
                        monitor_pid=MonitorPid,
                        user_state=UserState}};
    Other ->
      {error, monitor_start_failed, Other}
  end.


% Delegate calls, casts (other than ssa internal messages), info messages and temrination
% messages to the actor.

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

% Handle incoming user messages. These have been checked by the monitor to
% ensure that they conform to the MPST.
handle_cast(Msg = {Protocol, Role, {message, _, Sender, _, Op, Types, Payload}}, State) ->
  actor_info("Processing message ~p", [Msg], State),
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  % TODO: ssactor_handle_message currently just returns a new state.
  % Should we have some more complex callback here instead?
  NewUserState = Module:ssactor_handle_message(Sender, Op, Types, Payload,
                                           UserState,
                                           {Protocol, Role, State#actor_state.monitor_pid}),
  {noreply, State#actor_state{user_state=NewUserState}};
handle_cast(_Msg = {Protocol, Role, {become, Operation, Arguments}}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  NewUserState = Module:ssactor_become(Role, Operation, Arguments,
                                       {Protocol, Role, State#actor_state.monitor_pid}, UserState),
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
  actor_error("Actor terminating for reason ~p", [Reason], State),
  Module = State#actor_state.actor_type_name,
  MonitorPID = State#actor_state.monitor_pid,
  actor_type_registry:deregister_actor_instance(Module, MonitorPID),
  Module:terminate(Reason, State),
  ok.

% Public API
start(ModuleName, Args, Options) ->
  gen_server:start(ssa_gen_server, [ModuleName, Args], Options).

start_link(ModuleName, Args, Options) ->
  gen_server:start_link(ssa_gen_server, [ModuleName, Args], Options).

call(ServerRef, Message) ->
  gen_server:call(ServerRef, Message).

call(ServerRef, Message, Timeout) ->
  gen_server:call(ServerRef, Message, Timeout).

cast(ServerRef, Message) ->
  gen_server:cast(ServerRef, Message).

reply(ServerRef, Message) ->
  gen_server:reply(ServerRef, Message).