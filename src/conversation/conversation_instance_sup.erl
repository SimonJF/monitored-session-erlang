-module(conversation_instance_sup).
-define(CONV_SUP_PROC_NAME, conv_instance_sup).
-compile(export_all).
-behaviour(supervisor).

start_link() ->
  supervisor:start_link({global, ?CONV_SUP_PROC_NAME},
                        conversation_instance_sup, []).

start_conversation_instance(ProtocolName, Roles) ->
  supervisor:start_child({global, ?CONV_SUP_PROC_NAME}, [ProtocolName, Roles]).

init(_Args) ->
  SupTemplate = {conversation_instance,
                 {conversation_instance, start_link, []},
                 temporary, brutal_kill, worker, [conversation_instance]},
  {ok, {{simple_one_for_one, 2, 60}, [SupTemplate]}}.


