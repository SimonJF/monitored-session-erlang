-module(simple_sup).
-compile(export_all).
-behaviour(supervisor).

init(_Args) ->
  Actor1Proc = {supervised_actor1, {supervised_actor1,
                                    start,
                                    []},
                permanent, 5000, worker, [supervised_actor1]},
  Actor2Proc = {supervised_actor2, {supervised_actor2,
                                    start,
                                    []},
                permanent, 5000, worker, [supervised_actor2]},
  {ok, {{one_for_all, 2, 60}, [Actor1Proc, Actor2Proc]}}.
