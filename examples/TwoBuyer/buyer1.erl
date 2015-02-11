-module(buyer1).
-behaviour(session_actor).
-compile(export_all).

% Buyer 1:
%   Buyer 1 -> Server (title(String))
%   Server -> Buyer 1 (quote(Int))
%   Buyer 1 -> Buyer 2 (share(Int))
%   Choice:
%     Buyer2 -> Buyer 1 (accept(String))
%     Buyer2 -> Buyer 1 (retry())
%     Buyer2 -> Buyer 1 (quit())

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation in buyer1.~n", []),
  conversation:start_conversation(Monitor, "TwoBuyers", "A"),
  conversation:send(Monitor, ["S"], "title", ["String"], ["To Kill a Mockingbird"]),
  no_state. % We don't need no state round these parts

ssactor_handle_message(SenderRole, "quote", _, [QuoteInt], _State, Monitor) ->
  tbp_logger:info(buyer1, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  conversation:send(Monitor, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  no_state;
ssactor_handle_message(SenderRole, "accept", _, [Address], _State, _Monitor) ->
  tbp_logger:info(buyer1, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  no_state;
ssactor_handle_message(SenderRole, "retry", _, _, _State, _Monitor) ->
  tbp_logger:info(buyer1, "~s wants to retry", [SenderRole]),
  no_state;
ssactor_handle_message(SenderRole, "quit", _, _, _State, _Monitor) ->
  tbp_logger:info(buyer1, "~s wants to quit", [SenderRole]),
  no_state;
ssactor_handle_message(_SenderRole, Op, Types, Payload, _State, _Monitor) ->
  tbp_logger:err(buyer1, "Unhandled message: (~s, ~w, ~w)", [Op, Types, Payload]),
  no_state.

