-module(monitor_tests).
-include_lib("eunit/include/eunit.hrl").
-define(SPEC_DIRECTORY, "scribble_specs/").

simple_send_recv_role1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RequestResponse_Role1.scr",
  ProtocolName = "RequestResponse",
  RoleName = "Role1",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  RequestMessage = message:message(0, "Role1", ["Role2"], "Request", [], []),
  {ok, MonitorInstance1} = monitor:send(RequestMessage, MonitorInstance),
  ResponseMessage = message:message(1, "Role2", ["Role1"], "Response", [], []),
  {ok, MonitorInstance2} = monitor:recv(ResponseMessage, MonitorInstance1),
  ?assert(monitor:is_ended(MonitorInstance2)).

simple_send_recv_role1_bad_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RequestResponse_Role1.scr",
  ProtocolName = "RequestResponse",
  RoleName = "Role1",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  RequestMessage = message:message(0, "Role1", ["Role2"], "Request", [], []),
  {error, Error, MonitorInstance1} = monitor:recv(RequestMessage, MonitorInstance).

simple_send_recv_role2_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RequestResponse_Role2.scr",
  ProtocolName = "RequestResponse",
  RoleName = "Role2",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  RequestMessage = message:message(0, "Role1", ["Role2"], "Request", [], []),
  {ok, MonitorInstance1} = monitor:recv(RequestMessage, MonitorInstance),
  ResponseMessage = message:message(1, "Role2", ["Role1"], "Response", [], []),
  {ok, MonitorInstance2} = monitor:send(ResponseMessage, MonitorInstance1),
  ?assert(monitor:is_ended(MonitorInstance2)).

