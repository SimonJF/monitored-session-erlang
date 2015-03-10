-module(supervised_actor2).
-behaviour(ssa_gen_server).
-compile(export_all).

start() ->
  ssa_gen_server:start(supervised_actor2, [], []).

ssactor_init(_Args, Monitor) -> Monitor.

ssactor_handle_message(_SenderRole, "Hello", _, [], _State, _ConvKey) ->
  exit(dying),
  ok.

handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, Monitor) ->
  Tuple = {"SupervisedActors1", "Actor2", Monitor},
  io:format("In terminate for actor2. Tuple: ~p~n", [Tuple]),
  conversation:send(Tuple,
                    ["Actor1"], "Dying", [], []),
  ok.
