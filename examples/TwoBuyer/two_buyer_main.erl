-module(two_buyer_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, two_buyer_conf:config()),
  {ok, _ServerPid} = session_actor:spawn(server, []),
  {ok, _Buyer2Pid} = session_actor:spawn(buyer2, []),
  {ok, _Buyer1Pid} = session_actor:spawn(buyer1, []).
