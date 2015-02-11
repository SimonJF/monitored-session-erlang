-module(buyer2).
-behaviour(session_actor).
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

ssactor_handle_message(SenderRole, "quote", _, [QuoteInt], _State, _Monitor) ->
  tbp_logger:info(buyer2, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  no_state;
ssactor_handle_message(SenderRole, "share", _, [Share], _State, Monitor) ->
  tbp_logger:info(buyer2, "Received share quote (~p) from ~s", [Share, SenderRole]),
  if Share >= ?PRICE_THRESHOLD ->
       % Nah, we aint paying that
       tbp_logger:info(buyer2, "Rejected share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["A", "S"], "quit", [], []);
     Share < ?PRICE_THRESHOLD ->
       % We can afford it: accept, send address to buyer2 and server,
       % and retrieve the delivery date from the server
       tbp_logger:info(buyer2, "Accepted share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["A", "S"], "accept",
                         ["String"], ["Informatics Forum"])
  end,
  no_state;
ssactor_handle_message(SenderRole, "date", _, [DeliveryDate], _State, _Monitor) ->
  tbp_logger:info(buyer2, "Received delivery date of ~s from ~s", [DeliveryDate, SenderRole]),
  no_state;
ssactor_handle_message(_SenderRole, Op, Types, Payload, _State, _Monitor) ->
  tbp_logger:err(buyer2, "Unhandled message: (~s, ~w, ~w)", [Op, Types, Payload]),
  no_state.

