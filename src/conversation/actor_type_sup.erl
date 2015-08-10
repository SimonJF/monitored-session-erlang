-module(actor_type_sup).
-define(ACTOR_TYPE_SUP_PROC_NAME, actor_type_sup).
-compile(export_all).
-behaviour(supervisor).

start_link() ->
  supervisor:start_link({global, ?ACTOR_TYPE_SUP_PROC_NAME},
                        actor_type_sup, []).

start_actor_type(ActorModuleName, ProtocolRoleMap) ->
  supervisor:start_child({global, ?ACTOR_TYPE_SUP_PROC_NAME}, [ActorModuleName, ProtocolRoleMap]).

init(_Args) ->
  SupTemplate = {actor_type,
                 {actor_type, start_link, []},
                 temporary, brutal_kill, worker, [actor_type]},
  {ok, {{simple_one_for_one, 2, 60}, [SupTemplate]}}.


