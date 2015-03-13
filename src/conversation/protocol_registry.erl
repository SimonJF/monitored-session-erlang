-module(protocol_registry).
-behaviour(gen_server).
-compile(export_all).
-define(PROTOCOL_REGISTRY, ssa_protocol_registry).

%%% Registry for protocol processes.
%%% Maps protocol names to protocol processes.


start_link(Args) ->
  gen_server:start_link({local, ?PROTOCOL_REGISTRY}, protocol_registry, Args, []).

% Find the actors which are involved in the protocol, and devise a mapping
% from roles to actor types.

% Helper function to abstract around the irritating pattern of adding to a list
% in an orddict which may not be there
orddict_add(Key, Val, Dict) ->
  case orddict:find(Key, Dict) of
    {ok, List} -> orddict:store(Key, [Val|List], Dict);
    error -> orddict:store(Key, [Val], Dict)
  end.

add_actor_to_map(ActorModuleName, RoleNames, Dict) ->
  lists:foldl(fun(RoleName, RunningDict) ->
                  orddict_add(RoleName, ActorModuleName, RunningDict) end,
              Dict, RoleNames).

generate_role_actor_map(ProtocolName, Config) ->
  lists:foldl(fun({ActorModuleName, ProtocolMap}, Dict) ->
                  FindRes = lists:keyfind(ProtocolName, 1, ProtocolMap),
                  case FindRes of
                    {ProtocolName, RoleNames} ->
                      % Great, we've found it -- add to the dict
                      add_actor_to_map(ActorModuleName, RoleNames, Dict);
                    false ->
                      error_logger:warning_msg("Could not find protocol ~s in protocol map " ++
                                                "for actor ~p.~n",
                                                [ProtocolName, ActorModuleName]),
                      Dict
                  end end, orddict:new(), Config).

% Spawns a child, appending the ProtocolName |-> Pid mapping
% to the given dictionary.
% If there's an error starting, then we ignore it for now.
% I might revisit this later -- maybe it would be better just
% not to start and instead Let It Fail?
% Protocol Name: Name of the protocol
% RoleDict: Role -> Role AST mapping
% ProcDict: Dict of protocol processes
spawn_child(ProtocolName, RoleDict, ProcDict, Config) ->
  RoleActorMap = generate_role_actor_map(ProtocolName, Config),
  io:format("Role |-> actor map for PT~s: ~p~n", [ProtocolName, orddict:to_list(RoleActorMap)]),
  Result = gen_server:start(protocol_type, [ProtocolName, RoleDict, RoleActorMap], []),
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


spawn_children(ProtocolMappings, Config) ->
  orddict:fold(fun(ProtocolName, RoleDict, ProcDict) ->
                  spawn_child(ProtocolName, RoleDict, ProcDict, Config) end,
              orddict:new(),
              ProtocolMappings).


%% OTP Callback Functions

% Spawn processes for each of the protocol names
init([ProtocolMappings, Config]) ->
  ProtocolRegistry = spawn_children(ProtocolMappings, Config),
  {ok, ProtocolRegistry}.

handle_call({get_process_id, ProtocolName}, _From, ProtocolRegistry) ->
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
  error_logger:error_msg("ERROR: Process registry terminated, reason: ~p~n",
                        [Reason]).

% Don't need this
code_change(_PV, ProtocolRegistry, _Ex) ->
  {ok, ProtocolRegistry}.

% Internal API functions
get_protocol_pid(ProtocolName) ->
  gen_server:call(?PROTOCOL_REGISTRY, {get_process_id, ProtocolName}).

% Looks up a protocol name, sends a message if the protocol exists
with_protocol_process(ProtocolName, Func) ->
  ProtocolPidRes = get_protocol_pid(ProtocolName),
  case ProtocolPidRes of
    {ok, ProtocolPid} -> {ok, Func(ProtocolPid)};
    error -> {error, bad_protocol_name} % Couldn't find the protocol process
  end.

% Gets the monitor for a role in a given process
get_monitor(ProtocolName, RoleName) ->
  MonitorFunc =
    fun (ProtocolPid) ->
      MonitorRes = gen_server:call(ProtocolPid, {get_monitor, RoleName}),
      case MonitorRes of
        {ok, Monitor} -> {ok, Monitor};
        error -> {error, nonexistent_monitor} % Couldn't find the monitor
      end end,
  with_protocol_process(ProtocolName, MonitorFunc).


get_roles(ProtocolName) ->
  GetRoleFunc = fun (ProtocolPid) -> gen_server:call(ProtocolPid, get_roles) end,
  with_protocol_process(ProtocolName, GetRoleFunc).

start_invitation(ProtocolName, ConversationID, InitiatorRole, InitiatorPID) ->
  StartInviteFunc = fun (ProtocolPid) ->
                        gen_server:call(ProtocolPid, {begin_invitation, ConversationID,
                                                      InitiatorRole, InitiatorPID})
                    end,
  with_protocol_process(ProtocolName, StartInviteFunc).

invite_actor_direct(ProtocolName, ConversationID, RoleName, InviteeMonitorPID) ->
  InviteDirectFunc = fun (ProtocolPid) ->
                         gen_server:call(ProtocolPid,
                                         {delayed_invitation, InviteeMonitorPID,
                                          RoleName, ConversationID})
                     end,
  with_protocol_process(ProtocolName, InviteDirectFunc).
