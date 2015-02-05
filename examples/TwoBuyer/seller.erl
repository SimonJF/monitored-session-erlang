-module(seller).
-behaviour(session_actor).
-compile(export_all).
-define(PRICE, 40).
-define(DELIVERY_DATE, "Sometime in the future").

% seller:
% Buyer 1 -> seller (title(String))
% seller -> Buyer1, Buyer2 (quote(Int))
% Choice:
%   Buyer2 -> seller (accept(String))
%     seller -> Buyer2(date(String))
%   Buyer2 -> seller (retry())
%   Buyer2 -> seller (quit())

ssactor_init(_Args) -> no_state. % We don't need no state round these parts

ssactor_handle_msg(SenderRole, "title", _, [Title], _State, Monitor) ->
  tbp_logger:info(seller, "Received title ~s from ~s", [Title, SenderRole]),
  conversation:send(Monitor, ["Buyer1", "Buyer2"], "quote", ["Integer"], [?PRICE]),
  no_state;
ssactor_handle_msg(SenderRole, "accept", _, [Address], _State, Monitor) ->
  tbp_logger:info(seller, "~s accepted quote; received address ~s", [SenderRole, Address]),
  conversation:send(Monitor, ["Buyer2"], "date", ["String"], [?DELIVERY_DATE]),
  no_state;
ssactor_handle_msg(SenderRole, "retry", _, _, _State, _Monitor) ->
  tbp_logger:info(seller, "~s wants to retry", [SenderRole]),
  no_state;
ssactor_handle_msg(SenderRole, "quit", _, _, _State, _Monitor) ->
  tbp_logger:info(seller, "~s wants to quit", [SenderRole]),
  no_state;
ssactor_handle_msg(_SenderRole, Op, Types, Payload, _State, _Monitor) ->
  tbp_logger:err(seller, "Unhandled message: (~s, ~w, ~w)", [Op, Types, Payload]),
  no_state.

