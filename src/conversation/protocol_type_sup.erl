-module(protocol_type_sup).
-define(PROTOCOL_TYPE_SUP_PROC_NAME, protocol_type_sup).
-compile(export_all).
-behaviour(supervisor).

start_link() ->
  supervisor:start_link({global, ?PROTOCOL_TYPE_SUP_PROC_NAME},
                        protocol_type_sup, []).

start_protocol_type(ProtocolName, RoleDict, RoleActorMap) ->
  supervisor:start_child({global, ?PROTOCOL_TYPE_SUP_PROC_NAME}, [ProtocolName, RoleDict, RoleActorMap]).

init(_Args) ->
  SupTemplate = {protocol_type,
                 {protocol_type, start_link, []},
                 temporary, brutal_kill, worker, [protocol_type]},
  {ok, {{simple_one_for_one, 2, 60}, [SupTemplate]}}.


