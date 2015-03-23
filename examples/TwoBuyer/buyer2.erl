-module(buyer2).
-behaviour(buyer2).
-compile(export_all).
-define(PRICE_THRESHOLD, 50).

% Buyer 2:
%   Server -> Buyer 2 (quote(Int))
%   Buyer 1 -> Buyer 2 (share(Int))
%   Choice:
%     Buyer 2 -> Buyer 1, Server (accept(String))
%       Server -> Buyer 2 (date(String))
%     Buyer 2 -> Buyer 1, Server (retry())
%     Buyer 2 -> Buyer 1, Server (quit())

ssactor_init(_Args, _Monitor) -> no_state. % We don't need no state round these parts

ssactor_join(_, _, _, State) -> {accept, State}.
ssactor_conversation_established(_, _, _, _, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "quote", [QuoteInt], _State, _Monitor) ->
  actor_logger:info(buyer2, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "share", [Share], _State, Monitor) ->
  actor_logger:info(buyer2, "Received share quote (~p) from ~s", [Share, SenderRole]),
  if Share >= ?PRICE_THRESHOLD ->
       % Nah, we aint paying that
       actor_logger:info(buyer2, "Rejected share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["A", "S"], "quit", [], []);
     Share < ?PRICE_THRESHOLD ->
       % We can afford it: accept, send address to buyer2 and server,
       % and retrieve the delivery date from the server
       actor_logger:info(buyer2, "Accepted share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["A", "S"], "accept",
                         ["String"], ["Informatics Forum"])
  end,
  no_state;
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "date", [DeliveryDate], _State, _Monitor) ->
  actor_logger:info(buyer2, "Received delivery date of ~s from ~s", [DeliveryDate, SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "B", _CID, _SenderRole, Op, Payload, _State, _Monitor) ->
  actor_logger:err(buyer2, "Unhandled message: (~s, ~w)", [Op, Payload]),
  no_state.

