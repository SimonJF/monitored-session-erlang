-module(dealer).
-behaviour(ssa_gen_server).
-compile(export_all).


ssactor_init(_Args, _Monitor) ->
  no_state.

% Receiving the stock list
% P
ssactor_handle_message("StoreLoad", "Dealer", _CID,
                       _SenderRole, "request", [Item, Quantity], State, ConvKey) ->
  actor_logger:info(dealer, "Received restock request from seller: ~p~n", [Item]),
  conversation:send(ConvKey, ["Store"], "put", ["String", "Integer"],
                    [Item, Quantity]),
  State;
ssactor_handle_message("StoreLoad", "Dealer", _CID,
                       _SenderRole, "quit", _, State, Monitor) ->
  actor_logger:info(dealer, "Received quit instruction from store. ~p~n", []),
  conversation:send(Monitor, ["Store"], "acc", [], []),
  State.

ssactor_become(_, _, State) -> State.

