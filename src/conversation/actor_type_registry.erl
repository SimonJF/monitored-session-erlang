-module(protocol_registry).
-behaviour(gen_server).
-compile(export_all).

%%% Registry for actor types.
%%% Maps actor type names to actor type processes.
%%% Note: actor type processes != actor instances!

spawn_child(ActorTypeName, ProcDict) ->
  Result = gen_server:start("actor_type_process", [ActorTypeName], []),
  case Result of
    {ok, Pid} -> orddict:store(ActorTypeName, Pid, ProcDict);
    Error ->
      % For now, if there's a problem starting the process, then
      % log the error and do nothing. It might be worth retrying
      % later on.
      error_logger:error_msg("Error starting process for actor type ~s: ~p~n",
                             [ActorTypeName, Error]),
      ProcDict
  end.

spawn_children(ActorTypes) ->
  lists:foldl(fun(ActorType, ProcDict) ->
                  spawn_child(ActorType, ProcDict) end,
              orddict:new(),
              ActorTypes).


%% OTP Callback Functions

% Spawn processes for each of the protocol names
init([ProtocolMappings]) ->
  ActorTypeRegistry = spawn_children(ProtocolMappings),
  {ok, ActorTypeRegistry}.

handle_call({get_process_id, ActorTypeName}, From, ActorTypeRegistry) ->
  % Try and find the protocol name in the dictionary, returning either
  % {ok, Pid} or error
  Result = orddict:find(ActorTypeName, ActorTypeRegistry),
  {reply, Result, ActorTypeRegistry};
handle_call(Other, _From, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown call message in ActorTypeRegistry: ~p~n", [Other]),
  {noreply, ActorTypeRegistry}.

% There shouldn't really be any async messsages?
handle_cast(Other, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown cast message in ActorTypeRegistry: ~p~n", [Other]),
  {noreply, ActorTypeRegistry}.

% Nor info messages
handle_info(Other, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown info message in ActorTypeRegistry: ~p~n", [Other]),
  {noreply, ActorTypeRegistry}.

% Don't need this
code_change(_PV, ActorTypeRegistry, _Ex) ->
  {ok, ActorTypeRegistry}.
