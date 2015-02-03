-module(actor_type).
-behaviour(gen_server).
-compile(export_all).

-record(actor_type_state, {name,
                           actor_instances=[]}).


add_actor_instance(ActorInstancePid, ActorTypeState) ->
  ActorInstances = ActorTypeState#actor_type_state.actor_instances,
  NewActorInstances = ActorInstances ++ [ActorInstancePid],
  ActorTypeState#actor_type_state{actor_instances=NewActorInstances}.

remove_actor_instance(ActorInstancePid, ActorTypeState) ->
  ActorInstances = ActorTypeState#actor_type_state.actor_instances,
  NewActorInstances = lists:delete(ActorInstancePid, ActorInstances),
  ActorTypeState#actor_type_state{actor_instances=NewActorInstances}.

handle_invite_actor(ProtocolName, RoleName, ConversationID, ActorTypeState) ->
  ActorInstances = ActorTypeState#actor_type_state.actor_instances,
  Res = handle_invite_inner(ActorInstances, ProtocolName, RoleName, ConversationID),
  {reply, Res, ActorTypeState}.


handle_invite_inner([], ProtocolName, RoleName, _) ->
  % Booo, nothing available
  error_logger:warn_message("Unable to find endpoint for protocol~s role ~s.~n",
                            [ProtocolName, RoleName]),
  {error, no_registered_actor};
handle_invite_inner([Instance|Instances], ProtocolName, RoleName, ConversationID) ->
  % Try and invite the actor instance to fulfil the role.
  Res = gen_server:call(Instance,
                        {invitation, ProtocolName, RoleName, ConversationID}),
  case Res of
    % Wahey, it's added.
    ok -> ok;
    {error, Err} ->
      error_logger:info_message("PID ~p could not fulfil role ~s: error ~w.~n",
                                [Instance, RoleName, Err]),
      handle_invite_inner(Instances, ProtocolName, RoleName, ConversationID)
  end.

%% OTP Callback Functions
% Spawn processes for each of the protocol names
init([ActorTypeName]) ->
  ActorState = #actor_type_state{name=ActorTypeName, actor_instances=[]},
  {ok, ActorState}.
handle_call({register_actor, ActorInstancePid}, _From, ActorTypeState) ->
  NewState = add_actor_instance(ActorInstancePid, ActorTypeState),
  {noreply, NewState};
handle_call({deregister_actor, ActorInstancePid}, _From, ActorTypeState) ->
  NewState = remove_actor_instance(ActorInstancePid, ActorTypeState),
  {noreply, NewState};
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _From, ActorTypeState) ->
  handle_invite_actor(ProtocolName, RoleName, ConversationID, ActorTypeState);
handle_call(Other, _From, ActorTypeState) ->
  error_logger:error_msg("Unknown call message in ActorTypeState: ~p~n", [Other]),
  {noreply, ActorTypeState}.

% There shouldn't really be any async messsages?
handle_cast(Other, ActorTypeState) ->
  error_logger:error_msg("Unknown cast message in ActorTypeState: ~p~n", [Other]),
  {noreply, ActorTypeState}.

% Nor info messages
handle_info(Other, ActorTypeState) ->
  error_logger:error_msg("Unknown info message in ActorTypeState: ~p~n", [Other]),
  {noreply, ActorTypeState}.

% Don't need this
code_change(_PV, ActorTypeState, _Ex) ->
  {ok, ActorTypeState}.

terminate(Reason, State) ->
  error_logger:error_msg("ERROR: Actor type process for actor type ~w " ++
                         "terminated for reason ~w.~n",
                         [State#actor_type_state.name, Reason]),
  ok.

