-module(failure_handler).
-compile(export_all).

-record(failure_data, {conv_key,         %% Conversation Key
                       alive_roles,      %% Roles alive at session termination
                       dead_roles,       %% Roles dead at session termination
                       conv_properties,  %% Per-conversation properties
                       failure_reason}). %% Reason for failure


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_failure_handler(ConvKey,
                      AliveRoles,
                      DeadRoles,
                      ConvProperties,
                      FailureReason,
                      HandlerFunction) ->
  FailureData = #failure_data{conv_key=ConvKey, alive_roles=AliveRoles,
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
  ok.

start_handler_session(FailureData,
                      ProtocolName, InternalInvitations, ExternalInvitations) ->
  ok.

