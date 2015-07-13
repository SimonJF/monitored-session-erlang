-module(failure_handler).
-compile(export_all).

-record(failure_data, {conv_id,         %% Conversation Key
                       alive_roles,      %% Roles alive at session termination
                       dead_roles,       %% Roles dead at session termination
                       conv_properties,  %% Per-conversation properties
                       failure_reason}). %% Reason for failure


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks that it is safe to create a failure handling subsession.
% In order to do this, ensures that:
%  * InternalInvitations (intersect) DeadRoles = null
%  * All roles are filled
% Also, The Internal Dead could definitely be a film title
check_start_failure_subsession(InternalInvitations, ExternalInvitations,
                               ProtocolName, FailureData) ->
  InternalRoles = sets:from_list(InternalInvitations),
  DeadRoles = sets:from_list(FailureData#failure_data.dead_roles),

  % Dead role check (not strictly necessary, but saves time)
  InternalDead = sets:size(sets:intersection(InternalRoles, DeadRoles)),
  AllInternalRolesAlive = InternalDead == 0,
  if not AllInternalRolesAlive ->
       {error, {internal_invitees_dead, InternalDead}};
     AllInternalRolesAlive ->
       actor_monitor:check_subsession_roles_filled(InternalInvitations,
                                                   ExternalInvitations,
                                                   ProtocolName)
  end.


start_failure_handler(FailureData, ProtocolName, InternalInvitations,
                      ExternalInvitations, FailureHandlingFn) ->
  ConvID = FailureData#failure_data.conv_id,
  case check_start_failure_subsession(InternalInvitations, ExternalInvitations,
                                      ProtocolName, FailureData) of
    {ok, RoleSpecs} ->
      error_logger:info_msg("Starting failure handling subsession ~p~n", [ProtocolName]),
      SubsessionRes = conversation_instance:start_subsession(
                        ProtocolName, RoleSpecs, ConvID, undefined,
                        undefined, FailureHandlingFn),
      case SubsessionRes of
        {ok, SubsessionPID} ->
          conversation_instance:start_subsession_invitations(SubsessionPID, InternalInvitations,
                                                             ExternalInvitations);
        Other -> Other
      end;
    Other -> Other
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_failure_handler(ConvID,
                      AliveRoles,
                      DeadRoles,
                      ConvProperties,
                      FailureReason,
                      HandlerFunction) ->
  FailureData = #failure_data{conv_id=ConvID, alive_roles=AliveRoles,
                              dead_roles=DeadRoles,
                              conv_properties=ConvProperties,
                              failure_reason=FailureReason},
  HandlerFunction(FailureData).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts an error handling session.
% We can also add another failure handler to this, in case it fails.

start_handler_session(FailureData,
                      ProtocolName, InternalInvitations, ExternalInvitations,
                      FailureHandlingFunction) ->
  start_failure_handler(FailureData, ProtocolName, InternalInvitations,
                        ExternalInvitations, FailureHandlingFunction).

start_handler_session(FailureData,
                      ProtocolName, InternalInvitations, ExternalInvitations) ->
  start_failure_handler(FailureData, ProtocolName, InternalInvitations,
                        ExternalInvitations, undefined).

% Check if all roles are alive, and retry.the session.
retry() -> ok.

