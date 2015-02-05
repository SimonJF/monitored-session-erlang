-module(protocol_type).
-compile(export_all).
% Protocol type process. (AKA Protocol Manager, which makes more sense)
% Capabilities:
%  * Return monitors for process roles
%  * Keep a registry of actors which fulfil certain processes
%     * Register and deregister actors
%  * Send invitation messages to actors to enable them to fulfil roles
%
% State needed:
%  * Protocol name
%  * Role |-> RoleSpec mapping
%  * Role |-> Monitor mapping
%  * Role |-> Actor type mapping

-record(protocol_state, {protocol_name, % Protocol name (string)
                         role_names, % [Role]
                         role_specs, % Role |-> RoleSpec mappings (orddict)
                         monitors, % Role |-> Monitor mappings (orddict)
                         role_actor_mapping}). % Role |-> Actor Type

protocol_name(State) -> State#protocol_state.protocol_name.

generate_monitors(RoleSpecList, ProtocolName) ->
  lists:foldl(fun({Role, Spec}, MonitorDict) ->
                  MonitorRes = monitor:create_monitor(Spec),
                  case MonitorRes of
                    {ok, MonitorInstance} ->
                      orddict:store(Role, MonitorInstance, MonitorDict);
                    Other ->
                      error_logger:warning_msg("WARN: Could not generate monitor for " ++
                                               "protocol ~s, role ~s -- error: ~p~n",
                                               [ProtocolName, Role, Other])
                  end end, orddict:new(), RoleSpecList).


% Invite actor instances to partake in a conversation, given the conversation
% ID and the role mapping.
invite_actors(ConversationID, State) ->
  % For each role in turn, attempt to invite an actor.
  Roles = orddict:fetch_keys(State#protocol_state.role_specs),
  Res = invite_actors_inner(Roles, ConversationID, State),
  {reply, Res, State}.

invite_actors_inner([], _, _) ->
  % We're done!
  ok;
invite_actors_inner([Role|Roles], ConversationID, State) ->
  Res = invite_actor_role(Role, ConversationID, State),
  case Res of
    ok ->
      % Great, invite the next one
      invite_actors_inner(Roles, ConversationID, State);
    {error, Err} -> {error, Err} % Otherwise report the error and abort.
  end.

invite_actor_role(RoleName, ConversationID, State) ->
  ProtocolName = protocol_name(State),
  RoleActorMapping = State#protocol_state.role_actor_mapping,
  ActorTypeRes = orddict:find(RoleName, RoleActorMapping),
  case ActorTypeRes of
    {ok, ActorType} ->
      actor_type_registry:invite_actor_to_role(ActorType,
                                               ProtocolName,
                                               RoleName,
                                               ConversationID);
    error ->
      {error, no_registered_actor}
  end.

% OTP Callbacks

init([ProtocolName, RoleSpecs, RoleMapping]) ->
  Monitors = generate_monitors(orddict:to_list(RoleSpecs), ProtocolName),
  State = #protocol_state{protocol_name=ProtocolName,
                          role_names=orddict:fetch_keys(RoleSpecs),
                          role_specs=RoleSpecs,
                          monitors=Monitors,
                          role_actor_mapping=RoleMapping},

  {ok, State}.

handle_call({get_monitor, RoleName}, _From, State) ->
  % orddict:find's return type is as good as any -- {ok, Monitor}
  % if we have the monitor, error if not
  Reply = orddict:find(RoleName, State#protocol_state.monitors),
  {reply, Reply, State};
handle_call(get_roles, _From, State) ->
  {reply, State#protocol_state.role_names, State};
handle_call({begin_invitation, ConversationID}, _From, State) ->
  invite_actors(ConversationID, State);
handle_call(Other, _From, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled synchronous messsage ~p.~n",
                           [protocol_name(State), Other]),
  {noreply, State}.

handle_cast(Request, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled asynchronous messsage ~p.~n",
                           [protocol_name(State), Request]),
  {noreply, State}.


handle_info(Request, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled info messsage ~p.~n",
                           [protocol_name(State), Request]),
  {noreply, State}.


terminate(Reason, State) ->
  error_logger:error_msg("ERROR: Protocol process ~s terminated with reason ~p.~n",
                         [protocol_name(State), Reason]).

code_change(_Old, State, _Extra) ->
  {ok, State}.

