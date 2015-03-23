-module(seller).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(PRICE, 40).
-define(DELIVERY_DATE, "Sometime in the future").

% seller:
% Buyer 1 -> seller (title(String))
% seller -> A, B (quote(Int))
% Choice:
%   B -> seller (accept(String))
%     seller -> B(date(String))
%   B -> seller (retry())
%   B -> seller (quit())

ssactor_init(_Args, _Monitor) -> no_state. % We don't need no state round these parts

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_, _, _, _, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "title", [Title], _State, Monitor) ->
  actor_logger:info(seller, "Received title ~s from ~s", [Title, "TwoBuyers", "S", _CID, SenderRole]),
  conversation:send(Monitor, ["A", "B"], "quote", ["Integer"], [?PRICE]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "accept", [Address], _State, Monitor) ->
  actor_logger:info(seller, "~s accepted quote; received address ~s", [SenderRole, Address]),
  conversation:send(Monitor, ["B"], "date", ["String"], [?DELIVERY_DATE]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "retry", _, _State, _Monitor) ->
  actor_logger:info(seller, "~s wants to retry", ["TwoBuyers", SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "quit", _, _State, _Monitor) ->
  actor_logger:info(seller, "~s wants to quit", ["TwoBuyers", SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, _SenderRole, Op, Payload, _State, _Monitor) ->
  actor_logger:err(seller, "Unhandled message: (~s, ~w)", [Op, Payload]),
  no_state.

