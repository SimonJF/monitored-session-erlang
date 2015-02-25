-module(warehouse).
-behaviour(session_actor).

-record(warehouse_state, {item_db}).
-define(RESTOCK_AMOUNT, 5).
-define(DELIVERY_DATE, "Tomorrow").

get_item_db(State) -> State#warehouse_state.item_db.
update_item_db(State, DB) -> State#warehouse_state{item_db=DB}.

ssactor_init(_Args, Monitor) ->
  #warehouse_state{item_db=orddict:new()}.

restock(Monitor, ProductName, Quantity) ->
  conversation:become(Monitor, "Store", restock, [ProductName, ?RESTOCK_AMOUNT]).


% Purchase protocol -- we're the seller. Customer is the buyer.
ssactor_handle_message(SenderRole, "getStockList", _, [], State, Monitor) ->
  actor_logger:info(warehouse, "Purchase protocol: stock list requested ~s~n",
                    []),
  % Send back the list of what we have in stock
  Products = orddict:fetch_keys(State#warehouse_state.item_db),
  conversation:send(Monitor, [SenderRole], "stockList", ["StringList"], Products),
  State;
ssactor_handle_message(SenderRole, "buy", _, [ProductName], State, Monitor) ->
  % Check if it's in stock. If so, reply and decrement stock. If not, reply with an error
  ItemDB = get_item_db(State),
  case orddict:find(ProductName, ItemDB) of
    {ok, StockNumber} when StockNumber > 0 ->
      NewStockNumber = StockNumber - 1,
      NewDB = orddict:store(ProductName, NewStockNumber, ItemDB),
      % If we're out of stock, we'll need to restock.
      if NewStockNumber =< 0 ->
           restock(Monitor, ProductName, ?RESTOCK_AMOUNT)
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
ssactor_handle_message(_SenderRole, "quit", _, _, State, _) -> State. % Nothing doing


% ssactor_become: Rolename, Operation, Parameters
ssactor_become("Store", restock, [ItemName, Quantity], Monitor) ->
  actor_logger:info(warehouse, "StoreLoad protocol: sending restock request"),
  conversation:send(Monitor, "request", ["String", "Integer"], [ItemName, Quantity]).
% StoreLoad protocol
%ssactor_handle_message(SenderRole, "", ) -> ().


