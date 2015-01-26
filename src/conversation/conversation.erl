-module(conversation).
-compile(export_all).

%%% Conversation API.
%%% This is the outward-facing API for session initiation and monitoring.
%%% Additionally, it contains functions to set up the scaffolding and
%%% error kernel of the system, including registering server processes.
%%%

% Initialises the system. Must be called prior to doing anything else!
initialise(SpecDir, ActorTypes) ->
  ProtocolMappings = protocol_loader:local_protocol_files(SpecDir),
  % Start the protocol registry, and register it to the atom protocol_registry.
  % This will also start all of the protocol processes.
  {ok, _PRPid} = gen_server:start_link({local, ssa_protocol_registry},
                                      "protocol_registry", [ProtocolMappings], []),

  % Next: start the actor type registry.
  {ok, _ATRPid} = gen_server:start_link({local, ssa_actor_type_registry},
                                        "actor_type_registry", [ActorTypes], [])
  ok.
