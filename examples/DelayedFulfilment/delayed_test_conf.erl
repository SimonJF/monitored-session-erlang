-module(delayed_test_conf).
-export([config/0]).

config() ->
  [{actorA, "Actor A", [{"DelayedTest", "A"}]},
   {actorB, "Actor B", [{"DelayedTest", "B"}]},
   {actorC, "Actor C", [{"DelayedTest", "C"}]}].
