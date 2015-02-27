-module(warehouse).
-behaviour(session_actor).
-compile(export_all).
-record(warehouse_state, {item_db}).
-define(RESTOCK_AMOUNT, 5).
-define(DELIVERY_DATE, "Tomorrow").

get_item_db(State) -> State#warehouse_state.item_db.
update_item_db(State, DB) -> State#warehouse_state{item_db=DB}.

ssactor_init(_Args, Monitor) ->
  ItemDB = orddict:from_list([{"Communication and Concurrency", 1},
                              {"Types and Programming Languages", 10},
                              {"Category Theory", 10}]),
  conversation:start_conversation(Monitor, "StoreLoad", "Store"),
  % TODO: Need the possibility to return an error from ssactor_init
  #warehouse_state{item_db=ItemDB}.

restock(Monitor, ProductName, Quantity) ->
  actor_logger:info(warehouse, "Purchase protocol: restocking ~s~n",
                   [ProductName]),
  conversation:become(Monitor, "Store", restock, [ProductName, Quantity]).


% Purchase protocol -- we're the seller. Customer is the buyer.
ssactor_handle_message(SenderRole, "getStockList", _, [], State, Monitor) ->
  actor_logger:info(warehouse, "Purchase protocol: stock list requested~n",
                    []),
  % Send back the list of what we have in stock
  Products = orddict:fetch_keys(State#warehouse_state.item_db),
  conversation:send(Monitor, [SenderRole], "stockList", ["StringList"], [Products]),
  State;
ssactor_handle_message(SenderRole, "buy", _, [ProductName], State, Monitor) ->
  actor_logger:info(warehouse, "Purchase protocol: purchase of ~s requested ~n",
                    [ProductName]),

  % Check if it's in stock. If so, reply and decrement stock. If not, reply with an error
  ItemDB = get_item_db(State),
  case orddict:find(ProductName, ItemDB) of
    {ok, StockNumber} when StockNumber > 0 ->
      NewStockNumber = StockNumber - 1,
      NewDB = orddict:store(ProductName, NewStockNumber, ItemDB),
      % If we're out of stock, we'll need to restock.
      if NewStockNumber =< 0 ->
           restock(Monitor, ProductName, ?RESTOCK_AMOUNT);
           %ok;
         NewStockNumber > 0 -> ok
      end,
      conversation:send(Monitor, [SenderRole], "confirmation", ["String"], [?DELIVERY_DATE]),
      update_item_db(State, NewDB);
    {ok, StockNumber} when StockNumber =< 0 ->
      % We need to restock and return an error in the meantime
      restock(Monitor, ProductName, ?RESTOCK_AMOUNT),
      conversation:send(Monitor, "error", [SenderRole], ["String"], ["Product out of stock."]),
      State;
    error ->
      % We don't stock the item
      conversation:send(Monitor, "error", [SenderRole], ["String"], ["Item not sold by this warehouse."]),
      State
  end;
ssactor_handle_message(_SenderRole, "quit", _, _, State, _) -> State;
% StoreLoad protocol, TODO: pass protocol as a parameter to ssactor_handle_message?
ssactor_handle_message("Dealer", "put", _, [ProductName, Quantity], State, _Monitor) ->
  ItemDB = get_item_db(State),
  NewDB = case orddict:find(ProductName, ItemDB) of
    {ok, StockNumber} ->
      NewStockNumber = StockNumber + Quantity,
      orddict:store(ProductName, NewStockNumber, ItemDB);
    error -> orddict:store(ProductName, Quantity, ItemDB)
  end,
  update_item_db(State, NewDB);

% Dealer's accepted that we've quit. Nothing doing.
ssactor_handle_message("Store", "acc", _, _, _, State) -> State.


% ssactor_become: Rolename, Operation, Parameters
ssactor_become("Store", restock, [ItemName, Quantity], Monitor, State) ->
  actor_logger:info(warehouse, "StoreLoad protocol: sending restock request", []),
  conversation:send(Monitor, ["Dealer"], "request", ["String", "Integer"], [ItemName, Quantity]),
  State.
% StoreLoad protocol
%ssactor_handle_message(SenderRole, "", ) -> ().


