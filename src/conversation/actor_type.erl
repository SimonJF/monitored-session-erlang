-module(actor_type).
-behaviour(gen_server).
-compile(export_all).

-record(actor_type_state, {name,
                           actor_instances=[]}).

%% OTP Callback Functions

add_actor_instance(ActorInstancePid, ActorTypeState) ->
  ActorInstances = ActorTypeState#actor_type_state.actor_instances,
  NewActorInstances = ActorInstances ++ [ActorInstancePid],
  ActorTypeState#actor_type_state{actor_instances=NewActorInstances}.

remove_actor_instance(ActorInstancePid, ActorTypeState) ->
  ActorInstances = ActorTypeState#actor_type_state.actor_instances,
  NewActorInstances = lists:delete(ActorInstancePid, ActorInstances),
  ActorTypeState#actor_type_state{actor_instances=NewActorInstances}.

% Spawn processes for each of the protocol names
init([ActorTypeName]) ->
  ActorState = #actor_type_state{name=ActorTypeName, actor_instances=[]},
  {ok, ActorState}.
handle_call({register_actor, ActorInstancePid}, From, ActorTypeRegistry) ->
  NewState = add_actor_instance(ActorInstancePid, ActorTypeRegistry),
  {noreply, NewState};
handle_call({deregister_actor, ActorInstancePid}, From, ActorTypeRegistry) ->
  NewState = remove_actor_instance(ActorInstancePid, ActorTypeRegistry),
  {noreply, NewState};


handle_call(Other, _From, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown call message in ActorTypeRegistry: ~p~n", [Other]),
  {noreply, ActorTypeRegistry}.

% There shouldn't really be any async messsages?
handle_cast(Other, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown cast message in ActorTypeRegistry: ~p~n", [Other]),
  {noreply, ActorTypeRegistry}.

% Nor info messages
handle_info(Other, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown info message in ActorTypeRegistry: ~p~n", [Other]),
  {noreply, ActorTypeRegistry}.

% Don't need this
code_change(_PV, ActorTypeRegistry, _Ex) ->
  {ok, ActorTypeRegistry}.

