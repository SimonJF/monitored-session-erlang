-module(warehouse_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the warehouse protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, warehouse_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _} = ssa_gen_server:start(dealer, [], []),
  io:format("started dealer successfully~n"),
  {ok, _} = ssa_gen_server:start(warehouse, [], []),
  io:format("started warehouse successfully~n"),
  {ok, _} = ssa_gen_server:start(customer, [], []),
  io:format("started customer successfully~n"),
  ok.
