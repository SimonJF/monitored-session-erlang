-module(protocol_registry).
-define(PROTO_ETS_TABLE_NAME, protocol_registry_ets).

-behaviour(gen_server).
-compile(export_all).

% A table mapping protocol names to [(Role, Monitor)]

%%%% gen_server callbacks
init([ProtocolMapping]) ->
  init_ets(ProtocolMapping),
  {ok, no_state}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.
handle_cast(_, State) -> {stop, unexpected_cast, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

% Essentially a big ol' PROTO_ETS table.

% Populates the PROTO_ETS table.
% ProtocolMappings: ProtocolName |-> (Role name |-> Role spec)
populate_ets_table(ProtocolMapping) ->
  populate_ets_table_inner(orddict:to_list(ProtocolMapping)).
populate_ets_table_inner([]) -> ok;
populate_ets_table_inner([{PN, RoleDict}|XS]) ->
  Roles = lists:filtermap(fun({RoleName, RoleSpec}) ->
                             MonitorRes = monitor:create_monitor(RoleSpec),
                             case MonitorRes of
                               {ok, MonitorInstance} -> {true, {RoleName, MonitorInstance}};
                               Err ->
                                 error_logger:warning_msg("WARN: Could not generate monitor for " ++
                                                          "protocol ~s, role ~s -- error: ~p~n",
                                                          [PN, RoleName, Err])
                             end end, orddict:to_list(RoleDict)),
  ets:insert_new(?PROTO_ETS_TABLE_NAME, {PN, Roles}),
  populate_ets_table_inner(XS).

init_ets(ProtocolMapping) ->
  % We're in the fortunate position that we only write right at the start.
  % Afterwards, we can just blazing fast reads.
  _TID = ets:new(?PROTO_ETS_TABLE_NAME, [named_table, {read_concurrency, true}]),
  populate_ets_table(ProtocolMapping).



%%%% API

start_link(ProtocolMapping) ->
  gen_server2:start_link(protocol_registry, [ProtocolMapping], []).

get_roles_monitors(ProtocolName) ->
  ETSRes = ets:lookup(?PROTO_ETS_TABLE_NAME, ProtocolName),
  case ETSRes of
    [Res] -> element(2,Res);
    [] -> []
  end.


