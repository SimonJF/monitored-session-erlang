-module(dealer).
-behaviour(ssa_gen_server).
-compile(export_all).


ssactor_init(_Args, _Monitor) ->
  no_state.

ssactor_join(_, _, _, State) -> {accept, State}.
ssactor_conversation_established(_, _, _, _, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

% Receiving the stock list
% P
ssactor_handle_message("StoreLoad", "Dealer", _CID,
                       _SenderRole, "request", [Item, Quantity], State, ConvKey) ->
  actor_logger:info(dealer, "Received restock request from seller: ~p~n", [Item]),
  io:format("Dealer: restock request, ConvKey: ~p~n", [ConvKey]),
  conversation:send(ConvKey, ["Store"], "put", ["String", "Integer"],
                    [Item, Quantity]),
  State;
ssactor_handle_message("StoreLoad", "Dealer", _CID,
                       _SenderRole, "quit", _, State, ConvKey) ->
  actor_logger:info(dealer, "Received quit instruction from store. ~p~n", []),
  conversation:send(ConvKey, ["Store"], "acc", [], []),
  State.

ssactor_become(_, _, State) -> State.

