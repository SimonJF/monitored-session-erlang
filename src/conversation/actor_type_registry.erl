-module(actor_type_registry).
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


% Gets the PID for an actor type process with the given name
get_actor_type_pid(ActorTypeName, ActorTypeRegistry) ->
  orddict:find(ActorTypeName, ActorTypeRegistry).


% Resolves an actor type to a PID, and relays the given message.
% Returns either ok, or {error, error details}
actor_type_call(ActorTypeName, Message, ActorTypeRegistry) ->
  case get_actor_type_pid(ActorTypeName, ActorTypeRegistry) of
    {ok, ActorInstancePid} ->
      gen_server:call(ActorInstancePid, Message),
      ok;
    Other -> {error, actor_type_not_registered}
  end.


handle_register_actor(ActorTypeName, ActorInstancePid, ActorTypeRegistry) ->
  actor_type_call(ActorTypeName,
                  {register_actor, ActorInstancePid},
                  ActorTypeRegistry).

handle_deregister_actor(ActorTypeName, ActorInstancePid, ActorTypeRegistry) ->
  actor_type_call(ActorTypeName,
                  {deregister_actor, ActorInstancePid},
                  ActorTypeRegistry).

%% OTP Callback Functions

% Spawn processes for each of the protocol names
init([ActorTypes]) ->
  ActorTypeRegistry = spawn_children(ActorTypes),
  {ok, ActorTypeRegistry}.

handle_call({get_process_id, ActorTypeName}, From, ActorTypeRegistry) ->
  % Try and find the protocol name in the dictionary, returning either
  % {ok, Pid} or error
  Result = get_actor_type_pid(ActorTypeName, ActorTypeRegistry),
  {reply, Result, ActorTypeRegistry};
handle_call({register_actor, ActorType, ActorInstancePid}, From, ActorTypeRegistry) ->
  handle_register_actor(ActorType, ActorInstancePid, ActorTypeRegistry),
  {noreply, ActorTypeRegistry};
handle_call({deregister_actor, ActorType, ActorInstancePid}, From, ActorTypeRegistry) ->
  handle_deregister_actor(ActorType, ActorInstancePid, ActorTypeRegistry),
  {noreply, ActorTypeRegistry};


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

