-module(warehouse_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, warehouse_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _} = ssa_gen_server:spawn(dealer, []),
  io:format("Spawned dealer successfully~n"),
  {ok, _} = ssa_gen_server:spawn(warehouse, []),
  io:format("Spawned warehouse successfully~n"),
  {ok, _} = ssa_gen_server:spawn(customer, []),
  io:format("Spawned customer successfully~n"),
  ok.
