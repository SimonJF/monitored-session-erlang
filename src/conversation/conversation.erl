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

send({ProtocolName, RoleName, MonitorPID}, Recipients, MessageName, Types, Payload)  ->
  gen_server:call(MonitorPID, {send_msg, ProtocolName, RoleName, Recipients, MessageName,
                               Types, Payload}).

% Used to transition to another role.
become({_ProtocolName, _OldRole, MonitorPID}, RoleName, Operation, Arguments) ->
  gen_server:call(MonitorPID, {become, RoleName, Operation, Arguments}).

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
      ConversationProc = gen_server:start(conversation_instance, [ProtocolName, Roles], []),
      case ConversationProc of
        % And start the invitation system
        {ok, ConvPID} ->
          InviteRes = protocol_registry:start_invitation(ProtocolName, ConvPID, Role, MonitorPID),
          % Now, we'll know whether this has succeeded or not.
          % If it has, we can return a key to use for the rest of the conversation.
          % I'm wondering whether this is the best design. Perhaps an "after_invite" thing might work.
          case InviteRes of
            {ok, ok} -> {ok, {ProtocolName, Role, MonitorPID}};
            Err -> Err
          end;
        Err ->
          error_logger:error_msg("Error starting conversation for protocol ~s: ~p~n",
                                 [ProtocolName, Err]),
          Err
      end;
    Err ->
      error_logger:error_msg("Error starting conversation for protocol ~s: ~p~n",
                             [ProtocolName, Err]),
      Err
  end.

invite({ProtocolName, _RoleName, MonitorPID}, InviteeMonitorPID, InviteeRoleName) ->
  gen_server:call(MonitorPID, {send_delayed_invite, ProtocolName,
                               InviteeMonitorPID, InviteeRoleName}).

