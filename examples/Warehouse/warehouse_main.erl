-module(warehouse_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, warehouse_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _} = session_actor:spawn(dealer, []),
  io:format("Spawned dealer successfully~n"),
  {ok, _} = session_actor:spawn(warehouse, []),
  io:format("Spawned warehouse successfully~n"),
  {ok, _} = session_actor:spawn(customer, []),
  io:format("Spawned customer successfully~n"),
  ok.
