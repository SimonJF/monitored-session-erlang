-module(simple_supervisor_conf).
-export([config/0]).

config() ->
  [{supervised_actor1, "Actor1", [{"SupervisedActors1", "Actor1"}]},
   {supervised_actor2, "Actor2", [{"SupervisedActors1", "Actor2"}]}].

