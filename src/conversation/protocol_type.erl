-module(protocol_type).
-compile(export_all).
-behaviour(gen_server2).

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

% Checks whether a role is transient, given a role name and local protocol spec
is_role_transient(RoleName, {local_protocol, _, _, _, Roles, _}) ->
  is_role_transient_inner(RoleName, Roles);
is_role_transient(_, _) -> false.

is_role_transient_inner(_RN, []) -> false;
is_role_transient_inner(RoleName, [X|XS]) ->
  RoleTy = element(1, X),
  Name = element(2, X),
  if RoleName == Name ->
       (RoleTy == transient_role_decl) or (RoleTy == transient_role_decl_alias);
     RoleName =/= Name ->
       is_role_transient_inner(RoleName, XS)
  end.


% Invite actor instances to partake in a conversation, given the conversation
% ID and the role mapping.
invite_actors(ConversationID, InitiatorRole, InitiatorPID, State) ->
  % Firstly, invite the initiator to fulfil the role -- this needs to go through
  % for the rest to succeed
  Res = invite_actor_direct(InitiatorRole, InitiatorPID, ConversationID, State),
  case Res of
    ok ->
      % For each role in turn, attempt to invite an actor.
      % Only invite actors to fill non-transient roles, and roles which aren't
      % the initiator (as this is fulfilled directly)
      Roles = orddict:to_list(State#protocol_state.role_specs),
      FilteredRoles = lists:filtermap(fun({Role, RoleSpec}) ->
                                       RoleTransient = is_role_transient(Role, RoleSpec),
                                       ShouldInvite = (Role =/= InitiatorRole) and (not RoleTransient),
                                       if ShouldInvite ->
                                            {true, Role};
                                          true -> false
                                       end
                                   end, Roles),
      invite_actors_inner(FilteredRoles, ConversationID, State);
      % TODO: Check for error, broadcast error if so.
      % {reply, Res2, State};
    {error, Err} -> {error, {initiator_unable_to_fulfil, Err}}
  end.

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
    % Okay, so, explanation for this I guess.
    % We really want to lift the restriction that only one type of actor
    % can play a particular role in the protocol, as it seems a bit inflexible.
    % For example, we could have a customer or a warehouse being the Buyer in
    % the Purchase protocol.
    % Really, Neykova & Yoshida's actor / role discovery protocol makes sense
    % only when there's one actor type per protocol-role. So we take the first,
    % assuming that if we really need roles to be played by multiple actor types,
    % the roles are either initial, transient, or explicitly invited upon initiation.
    {ok, [ActorType|_XS]} ->
      actor_type_registry:invite_actor_to_role(ActorType,
                                               ProtocolName,
                                               RoleName,
                                               ConversationID);
    error ->
      {error, no_registered_actor_pt}
  end.

invite_actor_direct(RoleName, ActorMonitorPID, ConversationID, State) ->
  ProtocolName = protocol_name(State),
  gen_server:call(ActorMonitorPID, {invitation,
                                    ProtocolName,
                                    RoleName,
                                    ConversationID}).

init([ProtocolName, RoleSpecs, RoleMapping]) ->
  Monitors = generate_monitors(orddict:to_list(RoleSpecs), ProtocolName),
  State = #protocol_state{protocol_name=ProtocolName,
                          role_names=orddict:fetch_keys(RoleSpecs),
                          role_specs=RoleSpecs,
                          monitors=Monitors,
                          role_actor_mapping=RoleMapping},

  {ok, State}.

handle_delayed_invite(InviteeMonitorPID, RoleName, ConversationID, State) ->
  InviteRes = invite_actor_direct(RoleName, InviteeMonitorPID, ConversationID, State),
  {reply, InviteRes, State}.

handle_call({get_monitor, RoleName}, _From, State) ->
  % orddict:find's return type is as good as any -- {ok, Monitor}
  % if we have the monitor, error if not
  Reply = orddict:find(RoleName, State#protocol_state.monitors),
  {reply, Reply, State};
handle_call(get_roles, _From, State) ->
  {reply, State#protocol_state.role_specs, State};
handle_call(get_monitors, _From, State) ->
  {reply, State#protocol_state.monitors, State};
handle_call({delayed_invitation, InviteeMonitorPID, RoleName, ConversationID}, _From, State) ->
  handle_delayed_invite(InviteeMonitorPID, RoleName, ConversationID, State);
handle_call(Other, _From, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled synchronous messsage ~p.~n",
                           [protocol_name(State), Other]),
  {reply, ok, State}.

handle_cast({begin_invitation, ConversationID, InitiatorRole, InitiatorPID},
            State) ->
  _Res = invite_actors(ConversationID, InitiatorRole, InitiatorPID, State),
  {noreply, State};
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delayed_invitation(ProtocolPID, InviteeMonitorPID, RoleName, ConversationID) ->
  gen_server2:call(ProtocolPID, {delayed_invitation, InviteeMonitorPID,
                                 RoleName, ConversationID}).

get_roles(ProtocolPID) ->
  gen_server2:call(ProtocolPID, get_roles).


begin_invitation(ProtocolPID, ConversationID, InitiatorRole, InitiatorPID) ->
  gen_server2:cast(ProtocolPID, {begin_invitation, ConversationID,
                   InitiatorRole, InitiatorPID}).

get_monitor(ProtocolPID, RoleName) ->
  gen_server2:call(ProtocolPID, {get_monitor, RoleName}).

get_monitors(ProtocolPID) ->
  gen_server2:call(ProtocolPID, get_monitors).

