-module(seller).
-behaviour(session_actor).
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

ssactor_handle_message(SenderRole, "title", _, [Title], _State, Monitor) ->
  actor_logger:info(seller, "Received title ~s from ~s", [Title, SenderRole]),
  conversation:send(Monitor, ["A", "B"], "quote", ["Integer"], [?PRICE]),
  no_state;
ssactor_handle_message(SenderRole, "accept", _, [Address], _State, Monitor) ->
  actor_logger:info(seller, "~s accepted quote; received address ~s", [SenderRole, Address]),
  conversation:send(Monitor, ["B"], "date", ["String"], [?DELIVERY_DATE]),
  no_state;
ssactor_handle_message(SenderRole, "retry", _, _, _State, _Monitor) ->
  actor_logger:info(seller, "~s wants to retry", [SenderRole]),
  no_state;
ssactor_handle_message(SenderRole, "quit", _, _, _State, _Monitor) ->
  actor_logger:info(seller, "~s wants to quit", [SenderRole]),
  no_state;
ssactor_handle_message(_SenderRole, Op, Types, Payload, _State, _Monitor) ->
  actor_logger:err(seller, "Unhandled message: (~s, ~w, ~w)", [Op, Types, Payload]),
  no_state.

