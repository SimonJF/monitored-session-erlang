-module(simple_supervisor_main).
-compile(export_all).

main() ->
  conversation:initialise("scribble_specs",
                          simple_supervisor_conf:config()),
  {ok, SupRef} = supervisor:start_link(simple_sup, []),
  ChildList = supervisor:which_children(SupRef),
  {_, Actor1Pid, _, _} = lists:keyfind(supervised_actor1, 1, ChildList),
  gen_server:cast(Actor1Pid, start_conversation),
  ok.
