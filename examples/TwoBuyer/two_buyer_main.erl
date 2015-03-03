-module(two_buyer_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, two_buyer_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _ServerPid} = ssa_gen_server:spawn(seller, []),
  io:format("Spawned server successfully~n"),
  {ok, _Buyer2Pid} = ssa_gen_server:spawn(buyer2, []),
  io:format("Spawned buyer2 successfully~n"),
  {ok, _Buyer1Pid} = ssa_gen_server:spawn(buyer1, []),
  io:format("Spawned buyer1 successfully~n"),
  %receive
  %  {_Pid, blah} -> {}
  %end,
  ok.
