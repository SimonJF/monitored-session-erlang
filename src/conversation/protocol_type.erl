-module(protocol_type).

% Protocol type process. (AKA Protocol Manager, which makes more sense)
% Capabilities:
%  * Return monitors for process roles
%  * Keep a registry of actors which fulfil certain processes
%     * Register and deregister actors
%  * Send invitation messages to actors to enable them to fulfil roles
%
% State needed:
%  * Protocol name
%  * Role |-> RoleSpec mappings
%  * Role |-> Monitor mappings
%  * Role |-> Actor endpoint mappings

-record(protocol_state, {protocol_name, % Protocol name (string)
                         role_specs, % Role |-> RoleSpec mappings (orddict)
                         monitors, % Role |-> Monitor mappings (orddict)
                         actor_instances}). % Role |-> [Actor]

protocol_name(State) -> State#protocol_state.protocol_name.

generate_monitors(RoleSpecList, ProtocolName) ->
  lists:foldl(fun({Role, Spec}, MonitorDict) ->
                  MonitorRes = monitor:create_monitor(Spec),
                  case MonitorRes of
                    {ok, MonitorInstance} ->
                      orddict:store(Role, MonitorInstance, MonitorDict);
                    Other ->
                      error_logger:warning_msg("WARN: Could not generate monitor for " ++
                                               "protocol ~s, role ~s -- error: ~p~n",
                                               [ProtocolName, Role, Other])
                  end end, orddict:new(), RoleSpecList).


% OTP Callbacks

init([ProtocolName, RoleSpecs]) ->
  Monitors = generate_monitors(orddict:to_list(RoleSpecs), ProtocolName),
  State = #protocol_state{protocol_name=ProtocolName,
                          role_specs=RoleSpecs,
                          monitors=Monitors,
                          actor_instances=[]},

  {ok, State}.

handle_call({get_monitor, RoleName}, From, State) ->
  % orddict:find's return type is as good as any -- {ok, Monitor}
  % if we have the monitor, error if not
  Reply = orddict:find(RoleName, State#protocol_state.monitors),
  {reply, Reply, State};
handle_call(Other, _From, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled synchronous messsage ~p.~n",
                           [protocol_name(State), Other]),
  {noreply, State}.

handle_cast(Request, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled asynchronous messsage ~p.~n",
                           [protocol_name(State), Request]),
  {noreply, State}.


handle_info(Request, State) ->
  error_logger:warning_msg("WARN: Protocol process ~s received " ++
                           "unhandled info messsage ~p.~n",
                           [protocol_name(State), Request]),
  {noreply, State}.


terminate(Reason, State) ->
  error_logger:error_msg("ERROR: Protocol process ~s terminated with reason ~p.~n",
                         [protocol_name(State), Reason]).

code_change(_Old, State, _Extra) ->
  {ok, State}.

