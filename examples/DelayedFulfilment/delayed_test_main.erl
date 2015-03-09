-module(delayed_test_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

main() ->
  conversation:initialise(?SPEC_DIR, delayed_test_conf:config()),
  io:format("Initialised~n"),
  {ok, CI1PID} = ssa_gen_server:start(actorC, ["Instance 1"], []),
  io:format("started actor C, instance 1~n"),
  {ok, CI2PID} = ssa_gen_server:start(actorC, ["Instance 2"], []),
  io:format("started actor C, instance 2~n"),
  {ok, _} = ssa_gen_server:start(actorB, [CI1PID, CI2PID], []),
  io:format("started actor B, instance 2~n"),
  {ok, _} = ssa_gen_server:start(actorA, [], []),
  io:format("started actor A~n"),
  ok.

