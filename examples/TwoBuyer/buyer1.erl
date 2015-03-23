-module(buyer1).
-behaviour(ssa_gen_server).
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
  no_state.

% case ConvStartRes of
%   {ok, ConvKey} ->
%     io:format("ConvKey in ssactor_init: ~p~n", [ConvKey]),
%     conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
%   Err ->
%     error_logger:error_msg("Error starting conversation for protocol ~s (invite): ~p~n",
%                          ["TwoBuyers", Err])
% end,
% no_state. % We don't need no state round these parts

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "TwoBuyers" andalso RN == "A" ->
      conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:error(buyer1, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quote", [QuoteInt], _State, Monitor) ->
  actor_logger:info(buyer1, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  conversation:send(Monitor, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  no_state;
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "accept", [Address], _State, _Monitor) ->
  actor_logger:info(buyer1, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  no_state;
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "retry", _, _State, _Monitor) ->
  actor_logger:info(buyer1, "~s wants to retry", [SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quit", _, _State, _Monitor) ->
  actor_logger:info(buyer1, "~s wants to quit", [SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "A", _CID, _SenderRole, Op, Payload, _State, _Monitor) ->
  actor_logger:err(buyer1, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  no_state.

terminate(_, _) -> ok.
