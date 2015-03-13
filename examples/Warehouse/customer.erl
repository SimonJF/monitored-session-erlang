-module(customer).
-behaviour(ssa_gen_server).
-compile(export_all).


ssactor_init(_Args, Monitor) ->
  ConvRes = conversation:start_conversation(Monitor, "Purchase", "Buyer"),
  case ConvRes of
    {ok, ConvKey} ->
      actor_logger:info(customer, "Sending getStockList to seller", []),
      conversation:send(ConvKey, ["Seller"], "getStockList", [], []);
    Err ->
      actor_logger:error(customer, "Error starting conversation: ~p~n", [Err])
  end,
  no_state.

% Receiving the stock list
ssactor_handle_message("Purchase", "Buyer", _CID, _SenderRole, "stockList",
                       [Items], State, Monitor) ->
  actor_logger:info(customer, "Received stock list from seller: ~p~n", [Items]),
  conversation:send(Monitor, ["Seller"], "buy", ["String"], ["Communication and Concurrency"]),
  State;
ssactor_handle_message("Purchase", "Buyer", _CID, _SenderRole, "confirmation",
                       [DeliveryDate], State, Monitor) ->
  actor_logger:info(customer, "Received confirmation from seller, delivery date: ~p~n", [DeliveryDate]),
  conversation:send(Monitor, ["Seller"], "quit", [], []),
  State;
ssactor_handle_message("Purchase", "Buyer", _CID, _SenderRole, "error", [Err], State, Monitor) ->
  actor_logger:info(customer, "Couldn't purchase item, error: ~p~n", [Err]),
  conversation:send(Monitor, ["Seller"], "quit", [], []),
  State.

ssactor_become(_, _, _, _, State) -> State.

