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
initialise(SpecDir, Config) ->
  error_logger:info_msg("Initialising conversation system.~n", []),
  conversation_runtime_sup:start_link(SpecDir, Config).

teardown() ->
  conversation_runtime_sup:teardown().

send(ConvKey, Recipients, MessageName, Types, Payload) ->
  Res = actor_monitor:send_message(ConvKey, Recipients, MessageName, Types,
                                   Payload),
  case Res of
    ok -> ok;
    Err -> error(Err)
  end.

call({P, R, C, MonitorPID}, Recipient, MessageName, _, Payload) ->
  Res = actor_monitor:make_call(MonitorPID, P, R, C, Recipient,
                                MessageName, Payload),
  case Res of
    ok -> ok;
    Err -> error(Err)
  end.

% Used to transition to another role.
become({_, _, _, MonitorPID}, RegAtom, RoleName, Operation, Arguments) ->
  actor_monitor:become(MonitorPID, RegAtom, RoleName, Operation, Arguments).

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

set_conv_property({_, _, ConvID, _}, Key, Value) ->
  conversation_instance:set_property(ConvID, Key, Value).

unset_conv_property({_, _, ConvID, _}, Key) ->
  conversation_instance:unset_property(ConvID, Key).

get_conv_property({_, _, ConvID, _}, Key) ->
  conversation_instance:get_property(ConvID, Key).

end_conversation({_, _, ConvID, _}, Reason) ->
  conversation_instance:end_conversation(ConvID, Reason).


% Start a new subsession
start_subsession(ConvKey, ProtocolName, InternalInvitations, ExternalInvitations) ->
  actor_monitor:start_subsession(ConvKey, ProtocolName,
                                 InternalInvitations, ExternalInvitations).

% Subsession completed successfully
subsession_complete(ConvKey, Result) ->
  {_, _, CID, _} = ConvKey,
  conversation_instance:subsession_complete(CID, Result).

% An application logic exception occurred.
subsession_failed(ConvKey, FailureName) ->
  {_, _, CID, _} = ConvKey,
  conversation_instance:subsession_complete(CID, FailureName).

get_root_cid(ConvKey) ->
  {_, _, CID, _} = ConvKey,
  conversation_instance:get_root_pid(CID).
