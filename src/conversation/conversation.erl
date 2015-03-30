-module(conversation).
-compile(export_all).
-define(PROTOCOL_REGISTRY, ssa_protocol_registry).
-define(ACTOR_TYPE_REGISTRY, ssa_actor_type_registry).
-define(RUNTIME_PROC_NAME, conv_runtime_sup).

%%% Conversation API.
%%% This is the outward-facing API for session initiation and monitoring.
%%% Additionally, it contains functions to set up the scaffolding and
%%% error kernel of the system, including registering server processes.
%%%

% Initialises the system. Must be called prior to doing anything else!
initialise(SpecDir, Config) ->
  error_logger:info_msg("Initialising conversation system.~n", []),
  supervisor:start_link({local, ?RUNTIME_PROC_NAME},
                        conversation_runtime_sup, [SpecDir, Config]).

teardown() ->
  conversation_runtime_sup:teardown().

send(ConvKey, Recipients, MessageName, Types, Payload) ->
  Res = actor_monitor:send_message(ConvKey, Recipients, MessageName, Types,
                                   Payload),
  case Res of
    ok -> ok;
    Err -> error(Err)
  end.

% Used to transition to another role.
become(ConvKey, RegAtom, RoleName, Operation, Arguments) ->
  actor_monitor:become(ConvKey, RegAtom, RoleName, Operation, Arguments).

register_conversation(RegAtom, {ProtocolName, RoleName, ConvID, MonitorPID}) ->
  actor_monitor:register_become(MonitorPID, RegAtom, ProtocolName, RoleName, ConvID).

% Starts a conversation, assigning the initiator to the given role.
start_conversation(MonitorPID, ProtocolName, Role) ->
  % Retrieve the role names from the protocol reg server
  % Start a new conversation instance
  error_logger:info_msg("Starting conversation for protocol ~s.~n",
                         [ProtocolName]),
  RoleRes = protocol_registry:get_roles(ProtocolName),
  case RoleRes of
    {ok, Roles} ->
      % Next, need to start a new conversation process
      ConversationProc = conversation_instance:start(ProtocolName, Roles),
      case ConversationProc of
        % And start the invitation system
        {ok, ConvPID} ->
          protocol_registry:start_invitation(ProtocolName, ConvPID, Role, MonitorPID);
        Err ->
          actor_monitor:conversation_setup_failed(MonitorPID, ProtocolName,
                                                  Role, {bad_conversation_process, Err})
      end;
    Err ->
      actor_monitor:conversation_setup_failed(MonitorPID, ProtocolName, Role,
                                              {bad_role, Err})
  end,
  ok.

invite(ConvKey, InviteeMonitorPID, InviteeRoleName) ->
  actor_monitor:invite(ConvKey, InviteeMonitorPID, InviteeRoleName).

