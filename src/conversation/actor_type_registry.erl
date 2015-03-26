-module(actor_type_registry).
-behaviour(gen_server).
-compile(export_all).
-define(ACTOR_TYPE_REGISTRY, ssa_actor_type_registry).


%%% Registry for actor types.
%%% Maps actor type names to actor type processes.
%%% Note: actor type processes != actor instances!
start_link(Args) ->
  gen_server:start_link({local, ?ACTOR_TYPE_REGISTRY}, actor_type_registry, Args, []).


spawn_child(ActorModuleName, ProtocolRoleMap, ProcDict) ->
  Result = gen_server:start(actor_type, [ActorModuleName, ProtocolRoleMap], []),
  case Result of
    {ok, Pid} -> orddict:store(ActorModuleName, Pid, ProcDict);
    Error ->
      % For now, if there's a problem starting the process, then
      % log the error and do nothing. It might be worth retrying
      % later on.
      error_logger:error_msg("Error starting process for actor type ~p: ~p~n",
                             [ActorModuleName, Error]),
      error(starting_proc_failed),
      ProcDict
  end.

spawn_children(Config) ->
  lists:foldl(fun({ActorModuleName, ProtocolRoleMap}, ProcDict) ->
                  spawn_child(ActorModuleName, ProtocolRoleMap, ProcDict) end,
              orddict:new(),
              Config).


% Gets the PID for an actor type process with the given name
get_actor_type_pid(ActorTypeName, ActorTypeRegistry) ->
  orddict:find(ActorTypeName, ActorTypeRegistry).


% Resolves an actor type to a PID, and relays the given message.
% Returns either ok, or {error, error details}
actor_type_call(ActorTypeName, Message, ActorTypeRegistry) ->
  case get_actor_type_pid(ActorTypeName, ActorTypeRegistry) of
    {ok, ActorTypePid} ->
      gen_server:call(ActorTypePid, Message);
    Other ->
      io:format("Other in actor_type_call: ~p~n", [Other]),
      {error, actor_type_not_registered}
  end.


handle_register_actor(ActorTypeName, ActorInstancePid, ActorTypeRegistry) ->
  actor_type_call(ActorTypeName,
                  {register_actor, ActorInstancePid},
                  ActorTypeRegistry).

handle_deregister_actor(ActorTypeName, ActorInstancePid, ActorTypeRegistry) ->
  actor_type_call(ActorTypeName,
                  {deregister_actor, ActorInstancePid},
                  ActorTypeRegistry).

handle_invite_actor(ActorTypeName, ProtocolName, RoleName, ConversationID,
                    ActorTypeRegistry) ->
  Res = actor_type_call(ActorTypeName,
                        {invitation, ProtocolName, RoleName, ConversationID},
                        ActorTypeRegistry),
  {reply, Res, ActorTypeRegistry}.

%% OTP Callback Functions

% Spawn processes for each of the protocol names
init([Config]) ->
  ActorTypeRegistry = spawn_children(Config),
  {ok, ActorTypeRegistry}.

handle_call({get_process_id, ActorTypeName}, _From, ActorTypeRegistry) ->
  % Try and find the protocol name in the dictionary, returning either
  % {ok, Pid} or error
  Result = get_actor_type_pid(ActorTypeName, ActorTypeRegistry),
  {reply, Result, ActorTypeRegistry};
handle_call({register_actor, ActorType, ActorInstancePid}, _From, ActorTypeRegistry) ->
  handle_register_actor(ActorType, ActorInstancePid, ActorTypeRegistry),
  {reply, ok, ActorTypeRegistry};
handle_call({deregister_actor, ActorType, ActorInstancePid}, _From, ActorTypeRegistry) ->
  handle_deregister_actor(ActorType, ActorInstancePid, ActorTypeRegistry),
  {reply, ok, ActorTypeRegistry};
handle_call({invite_actor, ActorTypeName, ProtocolName, RoleName, CID},
            _From, ActorTypeRegistry) ->
  handle_invite_actor(ActorTypeName, ProtocolName, RoleName, CID, ActorTypeRegistry);
handle_call(Other, _From, ActorTypeRegistry) ->
  error_logger:error_msg("Unknown call message in ActorTypeRegistry: ~p~n", [Other]),
  {reply, ok, ActorTypeRegistry}.

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

terminate(Reason, _State) ->
  error_logger:error_msg("ERROR: Actor type registry terminating because of reason ~p~n.",
                         [Reason]),
  ok.


with_actor_process(ActorTypeName, Func) ->
  ActorPidRes = gen_server:call(?ACTOR_TYPE_REGISTRY,
                                {get_process_id, ActorTypeName}),
  case ActorPidRes of
    {ok, ProcessPid} -> Func(ProcessPid);
    Err ->
      error_logger:error_msg("ERROR: Could not find actor type process for AT ~p.~n",
                            [ActorTypeName]),
      Err
  end.

% Internal API

% Invite an actor to fulfil a role
invite_actor_to_role(ActorTypeName, ProtocolName, RoleName, ConversationID) ->
  gen_server:call(?ACTOR_TYPE_REGISTRY,
                  {invite_actor, ActorTypeName, ProtocolName, RoleName,
                   ConversationID}).

% Gets the protocol-role mapping for a given actor type
get_protocol_role_map(ActorTypeName) ->
  Func = fun (ProcessPid) ->
             gen_server:call(ProcessPid, get_protocol_role_map) end,
  with_actor_process(ActorTypeName, Func).


register_actor_instance(ActorType, ActorPid) ->
  gen_server:call(?ACTOR_TYPE_REGISTRY, {register_actor, ActorType, ActorPid}).

deregister_actor_instance(ActorType, ActorPid) ->
  gen_server:call(?ACTOR_TYPE_REGISTRY, {deregister_actor, ActorType, ActorPid}).


