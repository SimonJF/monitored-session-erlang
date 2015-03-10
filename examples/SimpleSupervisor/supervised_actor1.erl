-module(supervised_actor1).
-behaviour(ssa_gen_server).
-compile(export_all).

start() ->
  ssa_gen_server:start(supervised_actor1, [], []).

ssactor_init(_Args, Monitor) ->
  Monitor.

ssactor_handle_message(_SenderRole, "Dying", _, [], _State, ConvKey) ->
  % Wait for cleanup, then attempt to send a message
  timer:sleep(1000),
  conversation:send(ConvKey, ["Actor2"], "ProblematicMessage", [], []),
  ok.

handle_call(_Msg, _From, _Monitor) -> {reply, ok, no_state}.

handle_cast(start_conversation, Monitor) ->
  {ok, ConvKey} = conversation:start_conversation(Monitor, "SupervisedActors1", "Actor1"),
  conversation:send(ConvKey, ["Actor2"], "Hello", [], []),
  {noreply, Monitor}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.
