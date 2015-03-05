-module(actorA).
-compile(export_all).
-behaviour(ssa_gen_server).


ssactor_init(_Args, Monitor) -> 0.

ssactor_handle_message("A", 

handle_call(Msg, _, State) ->
  actor_logger:warning_msg(actorA, "Unhandled call, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  actor_logger:warning_msg(actorA, "Unhandled cast, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  actor_logger:warning_msg(actorA, "Unhandled info, msg = ~p~n", [Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

