-module(ssa_gen_server_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

ssactor_init(_Args, _Monitor) -> 0.

handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(_Request, From, State) ->
  if State == 0 ->
       {reply, ok, 1};
     State == 1 ->
       {reply, ok, 2, 5000};
     State == 2 ->
       ssa_gen_server:reply(From, ok),
       {noreply, 3};
     State == 3 ->
       ssa_gen_server:reply(From, ok),
       {noreply, 4, 5000}
  end.

handle_cast(_Request, State) ->
  if State == 4 ->
       {noreply, 5};
     State == 5 ->
       {noreply, 6, 5000}
  end.

get_and_assert(Pid, Target) ->
  % Now, check that the state is 6
  State = ssa_gen_server:call(Pid, get_state),
  ?assertEqual(Target, State),
  ok.

main_test() ->
  conversation:initialise(".", [{ssa_gen_server_test, "SSAGTS", []}]),
  {ok, Pid} = ssa_gen_server:start(ssa_gen_server_test, [], []),
  % Basic call
  ssa_gen_server:call(Pid, ok),
  get_and_assert(Pid, 1),
  % Call with timeout
  ssa_gen_server:call(Pid, ok),
  get_and_assert(Pid, 2),

  % Noreply call
  ssa_gen_server:call(Pid, ok),
  get_and_assert(Pid, 3),

  % Noreply Call with timeout
  ssa_gen_server:call(Pid, ok),
  get_and_assert(Pid, 4),

  % Cast
  ssa_gen_server:cast(Pid, ok),
  get_and_assert(Pid, 5),

  % Cast with timeout
  ssa_gen_server:cast(Pid, ok),
  get_and_assert(Pid, 6),
  ok.
