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

ssactor_init(_Args) ->
  % Start the conversation
%  conversation:start_conversation(self(), "TwoBuyers"), % TODO: self() needs to be monitor PID -- we don't have that
  no_state. % We don't need no state round these parts

ssactor_handle_msg(SenderRole, "quote", _, [QuoteInt], _State, Monitor) ->
  tbp_logger:info(buyer1, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  conversation:send(Monitor, ["B"], "share", ["Integer"], [100]),
  no_state;
ssactor_handle_msg(SenderRole, "accept", _, [Address], _State, _Monitor) ->
  tbp_logger:info(buyer1, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  no_state;
ssactor_handle_msg(SenderRole, "retry", _, _, _State, _Monitor) ->
  tbp_logger:info(buyer1, "~s wants to retry", [SenderRole]),
  no_state;
ssactor_handle_msg(SenderRole, "quit", _, _, _State, _Monitor) ->
  tbp_logger:info(buyer1, "~s wants to quit", [SenderRole]),
  no_state;
ssactor_handle_msg(_SenderRole, Op, Types, Payload, _State, _Monitor) ->
  tbp_logger:err(buyer1, "Unhandled message: (~s, ~w, ~w)", [Op, Types, Payload]),
  no_state.

