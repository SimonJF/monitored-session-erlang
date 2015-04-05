-module(conversation).
-compile(export_all).
-define(PROTOCOL_REGISTRY, ssa_protocol_registry).
-define(ACTOR_TYPE_REGISTRY, ssa_actor_type_registry).

%%% Conversation API.
%%% This is the outward-facing API for session initiation and monitoring.
%%% Additionally, it contains functions to set up the scaffolding and
%%% error kernel of the system, including registering server processes.

% Initialises the system. Must be called prior to doing anything else!
initialise(SpecDir, Config) ->
  error_logger:info_msg("Initialising conversation system.~n", []),
  conversation_runtime_sup:start_link(SpecDir, Config).

teardown() ->
  conversation_runtime_sup:teardown().

send(ConvKey, Recipients, MessageName, Types, Payload) ->
  Res = actor_proxy:send_message(ConvKey, Recipients, MessageName, Types,
                                 Payload),
  case Res of
    ok -> ok;
    Err -> error(Err)
  end.

% Used to transition to another role.
become({_, _, _, ProxyPID}, RegAtom, RoleName, Operation, Arguments) ->
  actor_proxy:become(ProxyPID, RegAtom, RoleName, Operation, Arguments).

register_conversation(RegAtom, {ProtocolName, RoleName, ConvID, ProxyPID}) ->
  actor_proxy:register_become(ProxyPID, RegAtom, ProtocolName, RoleName, ConvID).

% Starts a conversation, assigning the initiator to the given role.
start_conversation(ProxyPID, ProtocolName, Role) ->
  % Retrieve the role names from the protocol reg server
  % Start a new conversation instance
  error_logger:info_msg("Starting conversation for protocol ~s.~n",
                         [ProtocolName]),
  RoleRes = protocol_registry:get_roles(ProtocolName),
  MonitorRes = protocol_registry:get_monitors(ProtocolName),
  case {RoleRes, MonitorRes} of
    {{ok, Roles}, {ok, Monitors}} ->
      % Next, need to start a new conversation process
      ConversationProc = conversation_instance:start(ProtocolName, Roles, Monitors),
      case ConversationProc of
        % And start the invitation system
        {ok, ConvPID} ->
          protocol_registry:start_invitation(ProtocolName, ConvPID, Role, ProxyPID);
        Err ->
          actor_proxy:conversation_setup_failed(ProxyPID, ProtocolName,
                                                Role, {bad_conversation_process, Err})
      end;
    Err ->
      actor_proxy:conversation_setup_failed(ProxyPID, ProtocolName, Role,
                                              {bad_role, Err})
  end,
  ok.

invite(ConvKey, InviteeProxyPID, InviteeRoleName) ->
  actor_proxy:invite(ConvKey, InviteeProxyPID, InviteeRoleName).

end_conversation({_, _, ConvID, _}, Reason) ->
  conversation_instance:end_conversation(ConvID, Reason).
