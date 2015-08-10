% Conversation runtime process.
-module(conversation_runtime_sup).
-define(RUNTIME_PROC_NAME, conv_runtime_sup).
-compile(export_all).
-behaviour(supervisor).

init([SpecDir, Config]) ->
  ProtocolMappings = protocol_loader:load_protocol_files(SpecDir),
  ProtocolRegProc = {protocol_registry, {protocol_registry,
                                         start_link,
                                         [[ProtocolMappings, Config]]},
                     permanent, brutal_kill, worker, [protocol_registry]},
  ActorRegProc = {actor_type_registry, {actor_type_registry,
                                        start_link,
                                        [[Config]]},
                     permanent, brutal_kill, worker, [actor_type_registry]},
  ConvSupProc = {conversation_instance_sup, {conversation_instance_sup,
                                        start_link,
                                        []},
                     permanent, brutal_kill, worker, [conversation_instance_sup]},
  ActorSupProc = {actor_type_sup, {actor_type_sup,
                                        start_link,
                                        []},
                     permanent, brutal_kill, worker, [actor_type_sup]},

  ProtSupProc = {protocol_type_sup, {protocol_type_sup,
                                        start_link,
                                        []},
                     permanent, brutal_kill, worker, [protocol_type_sup]},


  {ok, {{one_for_one, 2, 60}, [ConvSupProc, ActorSupProc, ProtSupProc, ProtocolRegProc, ActorRegProc]}}.

start_link(SpecDir, Config) ->
  supervisor:start_link({global, ?RUNTIME_PROC_NAME},
                        conversation_runtime_sup, [SpecDir, Config]).

teardown() ->
  exit(whereis(?RUNTIME_PROC_NAME), kill).
