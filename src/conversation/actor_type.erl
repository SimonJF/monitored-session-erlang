-module(actor_type).
-behaviour(gen_server2).
-compile(export_all).

-record(actor_type_state, {name,
                           protocol_role_map,
                           actor_instances=[]}).


add_actor_instance(ActorInstancePid, State) ->
  ActorInstances = State#actor_type_state.actor_instances,
  NewActorInstances = ActorInstances ++ [ActorInstancePid],
  State#actor_type_state{actor_instances=NewActorInstances}.

remove_actor_instance(ActorInstancePid, State) ->
  ActorInstances = State#actor_type_state.actor_instances,
  NewActorInstances = lists:delete(ActorInstancePid, ActorInstances),
  State#actor_type_state{actor_instances=NewActorInstances}.

handle_invite_actor(ProtocolName, RoleName, ConversationID, State) ->
  ActorInstances = State#actor_type_state.actor_instances,
  Res = handle_invite_inner(ActorInstances, ProtocolName, RoleName, ConversationID),
  {reply, Res, State}.


handle_invite_inner([], ProtocolName, RoleName, _) ->
  % Booo, nothing available
  error_logger:warning_msg("Unable to find endpoint for protocol~s role ~s.~n",
                            [ProtocolName, RoleName]),
  {error, no_registered_actor};
handle_invite_inner([Instance|Instances], ProtocolName, RoleName, ConversationID) ->
  % Try and invite the actor instance to fulfil the role.
  Res = invite(Instance, ProtocolName, RoleName, ConversationID),
  case Res of
    % Wahey, it's added.
    ok -> ok;
    {error, Err} ->
      error_logger:info_msg("PID ~p could not fulfil role ~s: error ~w.~n",
                                [Instance, RoleName, Err]),
      handle_invite_inner(Instances, ProtocolName, RoleName, ConversationID)
  end.

%% OTP Callback Functions
% Spawn processes for each of the protocol names
init([ActorTypeName, ProtocolRoleMap]) ->
  ActorState = #actor_type_state{name=ActorTypeName,
                                 protocol_role_map=orddict:from_list(ProtocolRoleMap),
                                 actor_instances=[]},
  {ok, ActorState}.


handle_call({register_actor, ActorInstancePid}, _From, State) ->
  NewState = add_actor_instance(ActorInstancePid, State),
  {reply, ok, NewState};
handle_call({deregister_actor, ActorInstancePid}, _From, State) ->
  NewState = remove_actor_instance(ActorInstancePid, State),
  {reply, ok, NewState};
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _From, State) ->
  handle_invite_actor(ProtocolName, RoleName, ConversationID, State);
handle_call(get_protocol_role_map, _From, State) ->
  {reply, State#actor_type_state.protocol_role_map, State};
handle_call(Other, _From, State) ->
  error_logger:error_msg("Unknown call message in State: ~p~n", [Other]),
  {reply, ok, State}.

% There shouldn't really be any async messsages?
handle_cast(Other, State) ->
  error_logger:error_msg("Unknown cast message in State: ~p~n", [Other]),
  {noreply, State}.

% Nor info messages
handle_info(Other, State) ->
  error_logger:error_msg("Unknown info message in State: ~p~n", [Other]),
  {noreply, State}.

% Don't need this
code_change(_PV, State, _Ex) ->
  {ok, State}.

terminate(Reason, State) ->
  error_logger:error_msg("ERROR: Actor type process for actor type ~w " ++
                         "terminated for reason ~w.~n",
                         [State#actor_type_state.name, Reason]),
  ok.


%%%%
%%%% API
%%%%

invite(PID, ProtocolName, RoleName, ConversationID) ->
  gen_server2:call(PID, {invitation, ProtocolName, RoleName,
                              ConversationID}).

start_link(ActorModuleName, ProtocolRoleMap) ->
  gen_server2:start_link(actor_type, [ActorModuleName, ProtocolRoleMap], []).
