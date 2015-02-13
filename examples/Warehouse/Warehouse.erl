-module(warehouse).
-behaviour(session_actor).

-record(warehouse_state, {purchase_db}).


ssactor_init(_Args, Monitor) ->
  #warehouse_state{purchase_db=orddict:new()}.

% Purchase protocol: we're the buyer, and initiate the conversation
ssactor_handle_message(SenderRole, "authenticate", _, [AuthToken], State, Monitor) ->
  actor_logger:info(warehouse, "Purchase protocol: received auth token ~s~n",
                    [AuthToken]),
  % Now, request a quote
  conversation:send(Monitor, ["Seller"], "request", ["String"],
                    ["Types and Programming Languages"]),

% StoreLoad protocol
ssactor_handle_message(SenderRole, "", ) ->
