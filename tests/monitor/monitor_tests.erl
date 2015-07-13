-module(monitor_tests).
-include_lib("eunit/include/eunit.hrl").
-define(SPEC_DIRECTORY, "scribble_specs/").


% Simple sending and receiving.
% These three tests check that both projections of the same protocol work in the expected manner,
% sends and receives can only happen where they're meant to, and that both protocols are marked
% as ended at the end.
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
  {error, _Error} = monitor:recv(RequestMessage, MonitorInstance).

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


% Choice.
% Need to check:
%  * On a choice block, are both choices possible?
%  * Do choice blocks converge properly at the end?
%  * Do nested choice blocks work?

choice1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "ChoiceTest_Role1.scr",
  ProtocolName = "ChoiceTest",
  RoleName = "Role1",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  % Role1 -> Role2 (Hello)
  HelloMessage = message:message(0, "Role1", ["Role2"], "Hello", [], []),
  {ok, MonitorInstance1} = monitor:send(HelloMessage, MonitorInstance),
  % Role2 -> Role1 (World)
  WorldMessage = message:message(1, "Role2", ["Role1"], "World", [], []),
  {ok, MonitorInstance2} = monitor:recv(WorldMessage, MonitorInstance1),
  % Role1 -> Role2 (Hello) -- choice: branch 2
  HelloMessage1 = message:message(2, "Role1", ["Role2"], "Hello", [], []),
  {ok, MonitorInstance3} = monitor:send(HelloMessage1, MonitorInstance2),
  % Role2 -> Role1 (AmIInTrouble)
  AmIInTroubleMessage = message:message(3, "Role2", ["Role1"], "AmIInTrouble", [], []),
  {ok, MonitorInstance4} = monitor:recv(AmIInTroubleMessage, MonitorInstance3),
  % Role1 -> Role2 (Yes)
  YesMessage = message:message(4, "Role1", ["Role2"], "Yes", [], []),
  {ok, MonitorInstance5} = monitor:send(YesMessage, MonitorInstance4),
  % Role1 -> Role2 (Why)
  WhyMessage = message:message(5, "Role2", ["Role1"], "Why", [], []),
  {ok, MonitorInstance6} = monitor:recv(WhyMessage, MonitorInstance5),
  % Role1 -> Role2 (BrokeTheBuild) -- choice: branch 1
  BrokeTheBuildMessage = message:message(6, "Role1", ["Role2"], "BrokeTheBuild", [], []),
  {ok, MonitorInstance7} = monitor:send(BrokeTheBuildMessage, MonitorInstance6),
  % Role1 -> Role2 (Hello) -- choice: branch 2
  OkayFairEnoughMessage = message:message(7, "Role2", ["Role1"], "OkayFairEnough", [], []),
  {ok, MonitorInstance8} = monitor:recv(OkayFairEnoughMessage, MonitorInstance7),
  % Role1 -> Role2 (Hello) -- choice: branch 2
  CheerioOlChapMessage = message:message(8, "Role1", ["Role2"], "CheerioOlChap", [], []),
  {ok, MonitorInstance9} = monitor:send(CheerioOlChapMessage, MonitorInstance8),
  ?assert(monitor:is_ended(MonitorInstance9)).


rec1_test1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RecTest1_A.scr",
  ProtocolName = "RecTest1",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  RequestMessage = message:message(0, "A", ["B"], "x", [], []),
  {ok, MonitorInstance1} = monitor:send(RequestMessage, MonitorInstance),
  ResponseMessage = message:message(1, "B", ["A"], "y", [], []),
  {ok, MonitorInstance2} = monitor:recv(ResponseMessage, MonitorInstance1),
  ?assertNot(monitor:is_ended(MonitorInstance2)),
  % and request again to test recursion
  {ok, MonitorInstance3} = monitor:send(RequestMessage, MonitorInstance2),
  ?assertNot(monitor:is_ended(MonitorInstance3)).

rec1_test2_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RecTest1_B.scr",
  ProtocolName = "RecTest1",
  RoleName = "B",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  RequestMessage = message:message(0, "A", ["B"], "x", [], []),
  {ok, MonitorInstance1} = monitor:recv(RequestMessage, MonitorInstance),
  ResponseMessage = message:message(1, "B", ["A"], "y", [], []),
  {ok, MonitorInstance2} = monitor:send(ResponseMessage, MonitorInstance1),
  ?assertNot(monitor:is_ended(MonitorInstance2)),
  % and request again to test recursion
  {ok, MonitorInstance3} = monitor:recv(RequestMessage, MonitorInstance2),
  ?assertNot(monitor:is_ended(MonitorInstance3)).


rec2_test1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RecTest2_A.scr",
  ProtocolName = "RecTest2",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  StartMessage = message:message(0, "A", ["B"], "x", [], []),
  {ok, MonitorInstance1} = monitor:send(StartMessage, MonitorInstance),
  ChooseR2Message = message:message(1, "A", ["B"], "goToR2", [], []),
  {ok, MonitorInstance2} = monitor:send(ChooseR2Message, MonitorInstance1),
  ?assertNot(monitor:is_ended(MonitorInstance2)),
  ChooseR1Message = message:message(2, "A", ["B"], "quit", [], []),
  {ok, MonitorInstance3} = monitor:send(ChooseR1Message, MonitorInstance2),
  ?assert(monitor:is_ended(MonitorInstance3)).


rec2_test2_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RecTest2_A.scr",
  ProtocolName = "RecTest2",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  StartMessage = message:message(0, "A", ["B"], "x", [], []),
  {ok, MonitorInstance1} = monitor:send(StartMessage, MonitorInstance),
  ChooseR1Message = message:message(1, "A", ["B"], "goToR1", [], []),
  {ok, MonitorInstance2} = monitor:send(ChooseR1Message, MonitorInstance1),
  ?assertNot(monitor:is_ended(MonitorInstance2)),
  % and request again to test scope escape
  {ok, MonitorInstance3} = monitor:send(StartMessage, MonitorInstance2),
  ?assertNot(monitor:is_ended(MonitorInstance3)).

par1_test1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "ParTest1_A.scr",
  ProtocolName = "ParTest1",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  StartMessage = message:message(0, "A", ["B"], "Msg1", [], []),
  {ok, MonitorInstance1} = monitor:send(StartMessage, MonitorInstance),


  %error_logger:info_msg("Monitors before: ~p~n", [MonitorInstance1]),
  ParMsg1 = message:message(0, "B", ["A"], "Msg2", [], []),
  {ok, MonitorInstance2} = monitor:recv(ParMsg1, MonitorInstance1),
  % error_logger:info_msg("Monitors after: ~p~n", [MonitorInstance2]),

  ParMsg2 = message:message(0, "B", ["A"], "Msg3", [], []),
  {ok, MonitorInstance3} = monitor:recv(ParMsg2, MonitorInstance2),
  ParMsg3 = message:message(0, "A", ["B"], "Msg4", [], []),
  {ok, MonitorInstance4} = monitor:send(ParMsg3, MonitorInstance3),
  ParMsg4 = message:message(0, "A", ["B"], "Msg5", [], []),
  {ok, MonitorInstance5} = monitor:send(ParMsg4, MonitorInstance4),
  %error_logger:info_msg("Test1 monitor 5: ~p~n", [MonitorInstance5]),

  ParMsg5 = message:message(0, "A", ["B"], "Msg6", [], []),
  {ok, MonitorInstance6} = monitor:send(ParMsg5, MonitorInstance5),
  ?assert(monitor:is_ended(MonitorInstance6)).

par1_test2_test() ->
  Filename = ?SPEC_DIRECTORY ++ "ParTest1_A.scr",
  ProtocolName = "ParTest1",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  StartMessage = message:message(0, "A", ["B"], "Msg1", [], []),
  {ok, MonitorInstance1} = monitor:send(StartMessage, MonitorInstance),
  ParMsg1 = message:message(0, "B", ["A"], "Msg2", [], []),
  {ok, MonitorInstance2} = monitor:recv(ParMsg1, MonitorInstance1),
  ParMsg2 = message:message(0, "A", ["B"], "Msg4", [], []),
  {ok, MonitorInstance3} = monitor:send(ParMsg2, MonitorInstance2),
  ParMsg3 = message:message(0, "B", ["A"], "Msg3", [], []),
  {ok, MonitorInstance4} = monitor:recv(ParMsg3, MonitorInstance3),
  ParMsg4 = message:message(0, "A", ["B"], "Msg5", [], []),
  {ok, MonitorInstance5} = monitor:send(ParMsg4, MonitorInstance4),
  ParMsg5 = message:message(0, "A", ["B"], "Msg6", [], []),
  {ok, MonitorInstance6} = monitor:send(ParMsg5, MonitorInstance5),
  % error_logger:info_msg("MonitorInstance6: ~p~n", [MonitorInstance6]),
  ?assert(monitor:is_ended(MonitorInstance6)).

par1_badtest1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "ParTest1_A.scr",
  ProtocolName = "ParTest1",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  StartMessage = message:message(0, "A", ["B"], "Msg1", [], []),
  {ok, MonitorInstance1} = monitor:send(StartMessage, MonitorInstance),
  ParMsg1 = message:message(0, "B", ["A"], "Msg2", [], []),
  {ok, MonitorInstance2} = monitor:recv(ParMsg1, MonitorInstance1),
  ParMsg2 = message:message(0, "A", ["B"], "Msg4", [], []),
  {ok, MonitorInstance3} = monitor:send(ParMsg2, MonitorInstance2),
  BadMsg = message:message(0, "B", ["A"], "PINES", [], []),
  {error, _Error} = monitor:recv(BadMsg, MonitorInstance3).

par2_test1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "ParTest2_A.scr",
  ProtocolName = "ParTest2",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  StartMessage = message:message(0, "A", ["B"], "Msg1", [], []),
  % Iteration 1
  {ok, MonitorInstance1} = monitor:send(StartMessage, MonitorInstance),
  ParMsg1 = message:message(0, "B", ["A"], "Msg2", [], []),
  {ok, MonitorInstance2} = monitor:recv(ParMsg1, MonitorInstance1),
  ParMsg2 = message:message(0, "A", ["B"], "Msg4", [], []),
  {ok, MonitorInstance3} = monitor:send(ParMsg2, MonitorInstance2),
  ParMsg3 = message:message(0, "B", ["A"], "Msg3", [], []),
  {ok, MonitorInstance4} = monitor:recv(ParMsg3, MonitorInstance3),
  ParMsg4 = message:message(0, "A", ["B"], "Msg5", [], []),
  {ok, MonitorInstance5} = monitor:send(ParMsg4, MonitorInstance4),
%  error_logger:info_msg("Monitor 5, Partest2: ~p~n", [MonitorInstance5]),
  % Iteration 2
  ParMsg5 = message:message(0, "A", ["B"], "Msg1", [], []),
  {ok, MonitorInstance6} = monitor:send(ParMsg5, MonitorInstance5),
  ParMsg6 = message:message(0, "B", ["A"], "Msg2", [], []),
  {ok, MonitorInstance7} = monitor:recv(ParMsg6, MonitorInstance6),
  ParMsg7 = message:message(0, "A", ["B"], "Msg4", [], []),
  {ok, MonitorInstance8} = monitor:send(ParMsg7, MonitorInstance7),
  ParMsg8 = message:message(0, "B", ["A"], "Msg3", [], []),
  {ok, MonitorInstance9} = monitor:recv(ParMsg8, MonitorInstance8),
  ParMsg9 = message:message(0, "A", ["B"], "Msg5", [], []),
  {ok, _MonitorInstance10} = monitor:send(ParMsg9, MonitorInstance9).


