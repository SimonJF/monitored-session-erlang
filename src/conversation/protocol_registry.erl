-module(protocol_registry).
-behaviour(gen_server).
-compile(export_all).

%%% Registry for protocol processes.
%%% Maps protocol names to protocol processes.

% TODO: Treat this as a supervisor too, instead of
% treating each protocol process as part of the error
% kernel?


% Spawns a child, appending the ProtocolName |-> Pid mapping
% to the given dictionary.
% If there's an error starting, then we ignore it for now.
% I might revisit this later -- maybe it would be better just
% not to start and instead Let It Fail?
% Protocol Name: Name of the protocol
% RoleDict: Role -> Role AST mapping
% ProcDict: Dict of protocol processes
spawn_child(ProtocolName, RoleDict, ProcDict) ->
  Result = gen_server:start("protocol_process", [ProtocolName, RoleDict], []),
  case Result of
    {ok, Pid} -> orddict:store(ProtocolName, Pid, ProcDict);
    Error ->
      % For now, if there's a problem starting the process, then
      % log the error and do nothing. It might be worth retrying
      % later on.
      error_logger:error_msg("Error starting process for protocol ~s: ~p~n",
                             [ProtocolName, Error]),
      ProcDict
  end.


spawn_children(ProtocolMappings) ->
  lists:foldl(fun(ProtocolName, RoleDict, ProcDict) ->
                  spawn_child(ProtocolName, RoleDict, ProcDict) end,
              orddict:new(),
              ProtocolMappings).


%% OTP Callback Functions

% Spawn processes for each of the protocol names
init([ProtocolMappings]) ->
  ProtocolRegistry = spawn_children(ProtocolMappings),
  {ok, ProtocolRegistry}.

handle_call({get_process_id, ProtocolName}, From, ProtocolRegistry) ->
  % Try and find the protocol name in the dictionary, returning either
  % {ok, Pid} or error
  Result = orddict:find(ProtocolName, ProtocolRegistry),
  {reply, Result, ProtocolRegistry};
handle_call(Other, _From, ProtocolRegistry) ->
  error_logger:error_msg("Unknown call message in ProtocolRegistry: ~p~n", [Other]),
  {noreply, ProtocolRegistry}.

% There shouldn't really be any async messsages?
handle_cast(Other, ProtocolRegistry) ->
  error_logger:error_msg("Unknown cast message in ProtocolRegistry: ~p~n", [Other]),
  {noreply, ProtocolRegistry}.

% Nor info messages
handle_info(Other, ProtocolRegistry) ->
  error_logger:error_msg("Unknown info message in ProtocolRegistry: ~p~n", [Other]),
  {noreply, ProtocolRegistry}.

terminate(Reason, _ProtocolRegistry) ->
  error_logger:error_msg("ERROR: Process registry terminated.~n").

% Don't need this
code_change(_PV, ProtocolRegistry, _Ex) ->
  {ok, ProtocolRegistry}.
