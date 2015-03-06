-module(actorA).
-compile(export_all).
-behaviour(ssa_gen_server).


ssactor_init(_Args, Monitor) ->
  io:format("Starting conversation in actorA.", []),
  ConvStartRes = conversation:start_conversation(Monitor, "DelayedTest", "A"),
  case ConvStartRes of
    {ok, ConvKey} ->
      conversation:send(ConvKey, ["B"], "X", [], []);
    Err ->
      error_logger:error_msg("Error starting conversation for protocol ~s (invite): ~p~n",
                           ["DelayedTest", Err])
  end,
  no_state.

ssactor_handle_message(_, _, _, _, _, _) -> no_state.

handle_call(Msg, _, State) ->
  actor_logger:warn(actorA, "Unhandled call, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  actor_logger:warn(actorA, "Unhandled cast, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  actor_logger:warn(actorA, "Unhandled info, msg = ~p~n", [Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

