-module(server).
-behaviour(session_actor).
-compile(export_all).
-define(PRICE_THRESHOLD, 50).

% Buyer 2:
%   Server -> Buyer 2 (quote(Int))
%   Buyer 1 -> Buyer 2 (share(Int))
%   Choice:
%     Buyer 2 -> Buyer 1 (accept(String))
%       Server -> Buyer 2 (date(String))
%     Buyer 2 -> Buyer 1, Server (retry())
%     Buyer 2 -> Buyer 1, Server (quit())

init(_Args) -> no_state. % We don't need no state round these parts

ssactor_handle_msg(SenderRole, "title", _, [Title], _State, Monitor) ->
  tbp_logger:info(server, "Received title ~s from ~s", [Title, SenderRole]),
  conversation:send(Monitor),
  no_state;
ssactor_handle_msg(SenderRole, "share", _, [Share], _State, Monitor) ->
  tbp_logger:info(server, "~s received share quote (~p) from ~s", [Share, SenderRole]),
  if Share >= ?PRICE_THRESHOLD ->
       % Nah, we aint paying that
       tbp_logger:info(server, "Rejected share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["server"], "quit", [], []);
     Share < ?PRICE_THRESHOLD ->
       % We can afford it: accept, send address to server and server,
       % and retrieve the delivery date from the server
       tbp_logger:info(server, "Accepted share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["server", "Server"], "accept",
                         ["String"], ["Informatics Forum"])
  end,
  no_state;
ssactor_handle_msg(SenderRole, "date", _, [DeliveryDate], _State, _Monitor) ->
  tbp_logger:info(server, "Received delivery date of ~s from ~s", [DeliveryDate, SenderRole]),
  no_state;
ssactor_handle_msg(_SenderRole, Op, Types, Payload, _State, _Monitor) ->
  tbp_logger:err(server, "Unhandled message: (~s, ~w, ~w)", [Op, Types, Payload]),
  no_state.

