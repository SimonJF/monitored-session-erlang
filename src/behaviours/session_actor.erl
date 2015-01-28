-module(session_actor).

-export([behaviour_info/1]).
-behaviour(gen_server).

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
% ssactor_handle_message: SenderRole -> OperatorName -> PayloadTypes(?) -> Payload List
behaviour_info(callbacks) ->
    [{ssactor_init,1},
     {ssactor_handle_msg, 2}];
behaviour_info(_Other) ->
    undefined.


% gen_server callbacks
init([Module, ActorTypeName, AvailableRoles, UserArgs]) ->
  MonitorProcess = gen_server:start_link([self(), ActorTypeName, AvailableRoles]),
  case MonitorProcess of
    {ok, MonitorPid} ->
      UserState = Module:ssactor_init(UserArgs),
      {ok, #actor_state{monitor_pid=MonitorPid,
                        user_state=UserArgs,
                        actor_module=Module}};
    Other ->
      {error, monitor_start_failed, Other}
  end.


% We shouldn't have any synchronous calls -- log and ignore.
handle_call(Request, From, State) ->
  error_logger:warning_msg("WARN: Synchronous message received by session " ++
                           "actor process of type ~w, PID ~p: inoring.~n",
                           [State#actor_state.actor_type_name, self()]),
  {noreply, State}.


% Handle incoming user messages. These have been checked by the monitor to
% ensure that they conform to the MPST.
handle_cast(Msg = {message, Id, Sender, Op, Types, Payload}, State) ->
  error_logger:info_msg("INFO: Processing message ~w received by actor of " ++
                        "type ~w, with instance PID of ~w",
                       [Msg, State#actor_state.actor_type_name, self()]),
  Module = State#actor_state.actor_module,
  % TODO: ssactor_handle_message currently just returns a new state.
  % Should we have some more complex callback here instead?
  NewState = Module:ssactor_handle_message(Sender, Op, Types, Payload),
  {noreply, NewState};
handle_cast(Other, State) ->
  error_logger:warning_msg("WARN: Unhandled message ~w received by actor of " ++
                           "type ~w, with instance PID of ~w",
                           [Other, State#actor_state.actor_type_name, self()]),
  {noreply, State}.


% Info messages -- we don't do anything with these
handle_info(Msg, State) ->
  error_logger:warning_msg("WARN: Unhandled info msg ~w received by actor of " ++
                           "type ~w, with instance PID of ~w",
                           [Msg, State#actor_state.actor_type_name, self()]),
  {noreply, State}.


% We don't need this.
code_change(PreviousVersion, State, Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  error_logger:error_msg("ERROR: Actor of type ~w (instance PID ~p) " ++
                         "terminating because of reason ~p",
                         [State#actor_state.actor_type_name, self(), Reason]),
  ok.

% Public API

