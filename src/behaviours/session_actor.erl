-module(session_actor).
-behaviour(gen_server).

-compile(export_all).
-record(actor_state, {actor_type_name,
                      monitor_pid,
                      actor_module,
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
    [{ssactor_init,1},
     {ssactor_handle_msg, 5}];
behaviour_info(_Other) ->
    undefined.

log_msg(Func, Format, Args, State) ->
  InfoStr = "SSACTOR: Actor ~s, actor PID ~p, monitor PID ~p.",
  InfoArgs = [State#actor_state.actor_type_name,
              self(),
              State#actor_state.monitor_pid],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

actor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warn_message/2, Format, Args, State).

actor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_message/2, Format, Args, State).

actor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_message/2, Format, Args, State).


% gen_server callbacks
init([Module, UserArgs]) ->
  ProtocolRoleMap = actor_type_registry:get_protocol_role_map(Module),
  actor_type_registry:register_actor_instance(Module, self()),
  MonitorProcess = gen_server:start_link(actor_monitor,
                                         [self(),
                                          Module,
                                          ProtocolRoleMap], []),
  case MonitorProcess of
    {ok, MonitorPid} ->
      UserState = Module:ssactor_init(UserArgs),
      {ok, #actor_state{monitor_pid=MonitorPid,
                        user_state=UserState,
                        actor_module=Module}};
    Other ->
      {error, monitor_start_failed, Other}
  end.


% We shouldn't have any synchronous calls -- log and ignore.
handle_call(Request, _From, State) ->
  actor_warn("Received unhandled ynchronous message ~p", [Request], State),
  {noreply, State}.


% Handle incoming user messages. These have been checked by the monitor to
% ensure that they conform to the MPST.
handle_cast(Msg = {message, _, Sender, Op, Types, Payload}, State) ->
  actor_info("Processing message ~w", [Msg], State),
  Module = State#actor_state.actor_module,
  UserState = State#actor_state.user_state,
  % TODO: ssactor_handle_message currently just returns a new state.
  % Should we have some more complex callback here instead?
  NewState = Module:ssactor_handle_message(Sender, Op, Types, Payload, UserState),
  {noreply, NewState};
handle_cast(Msg, State) ->
  actor_warn("Received unhandled asynchronous message ~p", [Msg], State),
  {noreply, State}.


% Info messages -- we don't do anything with these
handle_info(Msg, State) ->
  actor_warn("Received unhandled info message ~p", [Msg], State),
  {noreply, State}.


% We don't need this.
code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  actor_error("Actor terminating for reason ~w", [Reason], State),
  Module = State#actor_state.actor_type_name,
  actor_type_registry:deregister_actor_instance(Module, self()),
  ok.

% Public API
spawn(ModuleName, Args) ->
  gen_server:start(ModuleName, Args, []).

