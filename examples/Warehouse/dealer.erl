-module(dealer).
-behaviour(session_actor).
-compile(export_all).


ssactor_init(_Args, _Monitor) ->
  no_state.

% Receiving the stock list
ssactor_handle_message(_SenderRole, "request", _, [Item, Quantity], State, Monitor) ->
  actor_logger:info(dealer, "Received restock request from seller: ~p~n", [Item]),
  conversation:send(Monitor, ["Store"], "put", ["String", "Integer"],
                    [Item, Quantity]),
  State;
ssactor_handle_message(_SenderRole, "quit", _, _, State, Monitor) ->
  actor_logger:info(dealer, "Received quit instruction from store. ~p~n", []),
  conversation:send(Monitor, ["Store"], "acc", [], []),
  State.

ssactor_become(_, _, State) -> State.

