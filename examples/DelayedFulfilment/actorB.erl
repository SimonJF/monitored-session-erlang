-module(actorB).
-compile(export_all).
-behaviour(ssa_gen_server).


ssactor_init([CInstance1MonitorID, CInstance2MonitorID], _Monitor) ->
  {0, CInstance1MonitorID, CInstance2MonitorID}.

ssactor_handle_message("A", "X", _, _, St = {0, CInst1PID, _}, ConvKey) ->
  % Invite Instance1 of C.
  conversation:invite(ConvKey, CInst1PID, "C"),
  conversation:send(ConvKey, ["C"], "Y", [], []),
  St;
ssactor_handle_message("C", "Z", _, _, {0, CInst1PID, CInst2PID}, ConvKey) ->
  conversation:invite(ConvKey, CInst2PID, "C"),
  conversation:send(ConvKey, ["C"], "Y", [], []),
  {1, CInst1PID, CInst2PID};
ssactor_handle_message("C", "Z", _, _, St = {1, _, _}, _ConvKey) ->
  St.


handle_call(Msg, _, State) ->
  actor_logger:warn(actorB, "Unhandled call, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_cast(Msg, State) ->
  actor_logger:warn(actorB, "Unhandled cast, msg = ~p~n", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  actor_logger:warn(actorB, "Unhandled info, msg = ~p~n", [Msg]),
  {noreply, State}.

terminate(_, _) -> ok.

