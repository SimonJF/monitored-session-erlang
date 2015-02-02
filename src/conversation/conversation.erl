-module(conversation).
-compile(export_all).
-define(PROTOCOL_REGISTRY, ssa_protocol_registry).
-define(ACTOR_TYPE_REGISTRY, ssa_actor_type_registry).

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
  {ok, _PRPid} = gen_server:start_link({local, ?PROTOCOL_REGISTRY},
                                      "protocol_registry", [ProtocolMappings], []),

  % Next: start the actor type registry.
  {ok, _ATRPid} = gen_server:start_link({local, ?ACTOR_TYPE_REGISTRY},
                                        "actor_type_registry", [ActorTypes], []),
  error_logger:info_message("Successfully initialised conversation system.~n", []),
  ok.


register_actor_instance(ActorType, ActorPid) ->
  gen_server:call(?ACTOR_TYPE_REGISTRY, {register_actor, ActorType, ActorPid}).

deregister_actor_instance(ActorType, ActorPid) ->
  gen_server:call(?ACTOR_TYPE_REGISTRY, {deregister_actor, ActorType, ActorPid}).


% Internal functions

send(MonitorPID, Recipients, MessageName, Types, Payload)  ->
  gen_server:call(MonitorPID, {send_msg, Recipients, MessageName, Types, Payload}).

% TODO: This doesn't currently automatically populate the role with the initiator.
% This means that this won't work unless theres n=1 actors of the initiator's type.
start_conversation(MonitorPID, ProtocolName) ->
  % Retrieve the role names from the protocol reg server
  % Start a new conversation instance
  gen_server:
