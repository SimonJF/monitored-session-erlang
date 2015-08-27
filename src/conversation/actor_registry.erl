-module(actor_registry).
-define(ACTOR_REGISTRY, actor_registry).
-define(ACTOR_ETS_TABLE1_NAME, actor_registry_ets_1). % ProtocolName |-> (Role |-> [PID])
-define(ACTOR_ETS_TABLE2_NAME, actor_registry_ets_2). % PID |-> [{Protocol, Role}].
-compile(export_all).

% Two tables:
% 1) Protocol name |e-> (Role |-> Actor PID)
% 2) Actor PID |e-> [{Protocol, Role}]

% Insert: retrieve orddict for protocol name, use orddict_add function
% to add the Actor PID to the list of active actors. Also add {P, R} to Actor PID table.
% Delete: Find list of {P, R}s, traverse, delete.


%%%% gen_server callbacks
%%% Config : Actor |-> [{Protocol, [Role]}]
init([Config]) ->
  init_ets(),
  {ok, orddict:from_list(expand_config(Config))}.

handle_call({register_actor, ActorType, ActorPID}, _, State) ->
  handle_register_actor(ActorType, ActorPID, State),
  {reply, ok, State};
handle_call(_, _, State) -> {stop, unexpected_call, State}.

handle_cast(_, State) -> {stop, unexpected_cast, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

% [{Actor, [{Protocol, [Role]}]}] -> [{Actor, [{Protocol, Role}]}]
expand_config(Config) ->
  lists:map(fun({Actor, Mapping}) ->
                {Actor, expand_mapping(Mapping)} end,
            Config).
% [{Protocol, [Role]}] -> [{Protocol, Role}]
expand_mapping(Mapping) ->
  lists:flatten(
    lists:map(fun({Protocol, RoleList}) ->
      lists:map(fun(Role) -> {Protocol, Role} end, RoleList) end, Mapping)).

init_ets() ->
  ets:new(?ACTOR_ETS_TABLE1_NAME, [named_table, public]),
  ets:new(?ACTOR_ETS_TABLE2_NAME, [named_table, public]),
  ok.

orddict_add(Key, Val, Dict) ->
  case orddict:find(Key, Dict) of
    {ok, List} -> orddict:store(Key, [Val|List], Dict);
    error -> orddict:store(Key, [Val], Dict)
  end.

% Mappings: [{Protocol, Role}]
% Registers an actor for the P-Rs in the Mappings list.
register_actor_ets(ActorName, ActorPID, Mappings) ->
  register_inner(Mappings, ActorName, ActorPID),
  ets:insert(?ACTOR_ETS_TABLE2_NAME,
             {ActorPID, Mappings}),
  ok.
register_inner([], _, _) ->
  ok;
register_inner([{P, R}|XS], ActorName, ActorPID) ->
  Res = ets:lookup(?ACTOR_ETS_TABLE1_NAME, P),
  case Res of
    [] -> ets:insert(?ACTOR_ETS_TABLE1_NAME,
                     {P, orddict_add(R, ActorPID, orddict:new())});
    [{P, RoleEndpointDict}] ->
      ets:insert(?ACTOR_ETS_TABLE1_NAME, {P, orddict_add(R, ActorPID, RoleEndpointDict)})
  end,
  register_inner(XS, ActorName, ActorPID).

handle_register_actor(ActorType, ActorPID, Config) ->
  case orddict:find(ActorType, Config) of
    {ok, Mappings} ->
      % Mappings: [{Protocol, Role}]
      register_actor_ets(ActorType, ActorPID, Mappings);
    _ ->
      error_logger:warning_msg("Couldn't find mapping when trying to register actor ~p~n",
                               [ActorType]),
      ok
  end.


%%%%% API

start_link(Config) ->
  gen_server2:start_link({global, ?ACTOR_REGISTRY}, actor_registry, [Config], []).

register_actor(ActorType, ActorPID) ->
  gen_server2:call({global, ?ACTOR_REGISTRY}, {register_actor, ActorType, ActorPID}).

deregister_actor(_, ActorPID) ->
  Res = ets:lookup(?ACTOR_ETS_TABLE2_NAME, ActorPID),
  % [[{P, R}]]
  case Res of
    [] -> ok;
    [Mappings] -> deregister_actor_from(element(2, Mappings), ActorPID)
  end.

deregister_actor_from([], _ActorPID) ->
  ok;
deregister_actor_from([{P, R}|XS], ActorPID) ->
  Res = ets:lookup(?ACTOR_ETS_TABLE1_NAME, P),
  case Res of
    [] -> ok;
    [DictRes] ->
      Dict = element(2, DictRes),
      RoleRes = orddict:find(R, Dict),
      case RoleRes of
        {ok, PIDList} ->
          FilteredList = lists:filter(fun(PID) -> PID =/= ActorPID end, PIDList),
          TermToStore = {P, orddict:store(R, FilteredList, Dict)},
          error_logger:info_msg("TermToStore: ~p", [TermToStore]),
          ets:insert(?ACTOR_ETS_TABLE1_NAME, TermToStore),
          deregister_actor_from(XS, ActorPID);
        error -> deregister_actor_from(XS, ActorPID)
      end
  end.


% Role |-> ActorPID dict
get_actors_for(ProtocolName) ->
  ETSRes = ets:lookup(?ACTOR_ETS_TABLE1_NAME, ProtocolName),
  case ETSRes of
    [Res] -> element(2,Res);
    [] -> orddict:new()
  end.
