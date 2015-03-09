-module(actorC).
-compile(export_all).
-behaviour(ssa_gen_server).


ssactor_init([Name], _Monitor) -> Name.

ssactor_handle_message("B", "Y", _, _, Name, ConvKey) ->
  actor_logger:info(actorC, "Message delivered to actorC, instance name = ~s~n",
                        [Name]),
  conversation:send(ConvKey, ["B"], "Z", [], []),
  Name.

handle_call(Msg, _, State) ->
  actor_logger:warn(actorC, "Unhandled call, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  actor_logger:warn(actorC, "Unhandled cast, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  actor_logger:warn(actorC, "Unhandled info, msg = ~p~n", [Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

