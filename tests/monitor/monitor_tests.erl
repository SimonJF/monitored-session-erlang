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
  % error_logger:info_msg("MonitorInstance3 : ~p~n", [MonitorInstance3]),

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
  %%%%%%
  % error_logger:info_msg("Monitor 6, partest2: ~p~n", [MonitorInstance6]),
  %%%
  {ok, MonitorInstance7} = monitor:recv(ParMsg6, MonitorInstance6),
  ParMsg7 = message:message(0, "A", ["B"], "Msg4", [], []),
  {ok, MonitorInstance8} = monitor:send(ParMsg7, MonitorInstance7),
  ParMsg8 = message:message(0, "B", ["A"], "Msg3", [], []),
  {ok, MonitorInstance9} = monitor:recv(ParMsg8, MonitorInstance8),
  ParMsg9 = message:message(0, "A", ["B"], "Msg5", [], []),
  {ok, _MonitorInstance10} = monitor:send(ParMsg9, MonitorInstance9).

subsession_test_setup() ->
  % Get all the boring junk out of the way first
  Filename = ?SPEC_DIRECTORY ++ "TravelBookingSplit_TravelAgent.scr",
  ProtocolName = "BookTravel",
  RoleName = "TravelAgent",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  ?assertNot(monitor:is_ended(MonitorInstance)),
  Msg1 = message:message(0, "Customer", ["TravelAgent"], "customerRequest",
                         [], [blah]),
  {ok, MonitorInstance1} = monitor:recv(Msg1, MonitorInstance),
  Msg2 = message:message(0, "TravelAgent", ["FlightBookingService"], "flightInfoRequest",
                         [], [blah]),
  Msg3 = message:message(0, "TravelAgent", ["HotelBookingService"], "hotelInfoRequest",
                         [], [blah]),
  {ok, MonitorInstance2} = monitor:send(Msg2, MonitorInstance1),
  {ok, MonitorInstance3} = monitor:send(Msg3, MonitorInstance2),
  Msg4 = message:message(0, "FlightBookingService", ["TravelAgent"], "flightInfoResponse",
                         [], [blah, blah]),
  Msg5 = message:message(0, "HotelBookingService", ["TravelAgent"], "hotelInfoResponse",
                         [], [blah]),
  {ok, MonitorInstance4} = monitor:recv(Msg4, MonitorInstance3),
  {ok, MonitorInstance5} = monitor:recv(Msg5, MonitorInstance4),
  Msg6 = message:message(0, "TravelAgent", ["Customer"], "customerResponse",
                         [], [blah, blah, blah]),
  {ok, MonitorInstance6} = monitor:send(Msg6, MonitorInstance5),
  Msg7 = message:message(0, "Customer", ["TravelAgent"], "proceedWithBooking",
                         [], [blah, blah, blah]),
  monitor:recv(Msg7, MonitorInstance6).

% Basic case -- all subsessions work fine
subsession_test1_test() ->
  {ok, MonitorInstance} = subsession_test_setup(),
  {ok, MonitorInstance1} = monitor:start_subsession("PerformBooking", ["TravelAgent", "Customer"],
                                                    ["FlightBookingService", "HotelBookingService"], MonitorInstance),
  {ok, MonitorInstance2} = monitor:subsession_success(MonitorInstance1),
  io:format("MonitorInstance2: ~p~n", [MonitorInstance2]),
  {ok, MonitorInstance3} = monitor:start_subsession("PerformPayment", ["TravelAgent"], ["PaymentProcessor"], MonitorInstance2),
  {ok, MonitorInstance4} = monitor:subsession_success(MonitorInstance3),
  Msg1 = message:message(0, "TravelAgent", ["Customer"], "confirmation", [], []),
  {ok, MonitorInstance5} = monitor:send(Msg1, MonitorInstance4),
  ?assert(monitor:is_ended(MonitorInstance5)).

% Check failure handling blocks
subsession_test2_test() ->
  {ok, MonitorInstance} = subsession_test_setup(),
  {ok, MonitorInstance1} = monitor:start_subsession("PerformBooking", ["TravelAgent", "Customer"],
                                                    ["FlightBookingService", "HotelBookingService"], MonitorInstance),
  {ok, MonitorInstance2} = monitor:subsession_failure("BookingFailure", MonitorInstance1),
  Msg1 = message:message(0, "TravelAgent", ["Customer"], "bookingFail", [], []),
  {ok, MonitorInstance3} = monitor:send(Msg1, MonitorInstance2),
  {ok, _MonitorInstance4} = monitor:start_subsession("CancelBookings", ["TravelAgent"],
                                                     ["FlightBookingService", "HotelBookingService"], MonitorInstance3).
% Check start session monitoring works
% Bad name
subsession_bad1_test() ->
  {ok, MonitorInstance} = subsession_test_setup(),
  {error, _} = monitor:start_subsession("PerformBookingWrongName", ["TravelAgent", "Customer"],
                                        ["FlightBookingService", "HotelBookingService"], MonitorInstance).

% Check start session monitoring works
% Bad Internal
subsession_bad2_test() ->
  {ok, MonitorInstance} = subsession_test_setup(),
  {error, _} = monitor:start_subsession("PerformBooking", ["Customer"],
                                        ["TravelBooking", "FlightBookingService", "HotelBookingService"], MonitorInstance).


% Subsession where we assume success, therefore don't have explicit success block
subsession_initiates1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "SubsessionInitiates1Test_A.scr",
  ProtocolName = "SubsessionInitiates1Test",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  Msg1 = message:message(0, "A", ["B"], "X", [], []),
  {ok, MonitorInstance1} = monitor:send(Msg1, MonitorInstance),
  {ok, MonitorInstance2} = monitor:start_subsession("Subsession", ["A", "B"], ["C"], MonitorInstance1),
  {ok, MonitorInstance3} = monitor:subsession_success(MonitorInstance2),
  Msg2 = message:message(0, "A", ["B"], "Y", [], []),
  {ok, MonitorInstance4} = monitor:send(Msg2, MonitorInstance3),
  ?assert(monitor:is_ended(MonitorInstance4)).


rec_reachability_test1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RecReachabilityTest_A.scr",
  ProtocolName = "RecReachabilityTest",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  true = monitor:is_role_reachable("B", MonitorInstance),
  true = monitor:is_role_reachable("C", MonitorInstance),

  Msg1 = message:message(0, "A", ["B"], "hello", [], []),
  {ok, MonitorInstance1} = monitor:send(Msg1, MonitorInstance),

  % Take first branch. First branch is recursive, so both B and C should be reachable
  Msg2 = message:message(0, "B", ["A"], "X", [], []),
  {ok, MonitorInstance2} = monitor:recv(Msg2, MonitorInstance1),
  true = monitor:is_role_reachable("B", MonitorInstance2),
  true = monitor:is_role_reachable("C", MonitorInstance2).

rec_reachability_test2_test() ->
  Filename = ?SPEC_DIRECTORY ++ "RecReachabilityTest_A.scr",
  ProtocolName = "RecReachabilityTest",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),
  true = monitor:is_role_reachable("B", MonitorInstance),
  true = monitor:is_role_reachable("C", MonitorInstance),

  Msg1 = message:message(0, "A", ["B"], "hello", [], []),
  {ok, MonitorInstance1} = monitor:send(Msg1, MonitorInstance),

  % Take second branch. Not recursive, so C should be reachable but not B.
  Msg2 = message:message(0, "B", ["A"], "Z", [], []),
  {ok, MonitorInstance2} = monitor:recv(Msg2, MonitorInstance1),
  false = monitor:is_role_reachable("B", MonitorInstance2),
  true = monitor:is_role_reachable("C", MonitorInstance2).

par_reachability_test1_test() ->
  Filename = ?SPEC_DIRECTORY ++ "ParReachabilityTest_A.scr",
  ProtocolName = "ParReachabilityTest",
  RoleName = "A",
  {ok, MonitorInstance} = monitor:create_monitor(Filename, ProtocolName, RoleName),

  % B, C, and D should be reachable to start
  true = monitor:is_role_reachable("B", MonitorInstance),
  true = monitor:is_role_reachable("C", MonitorInstance),
  true = monitor:is_role_reachable("D", MonitorInstance),

  % Next, send X() to B: only C and D should be reachable after
  Msg1 = message:message(0, "A", ["B"], "X", [], []),
  {ok, MonitorInstance1} = monitor:send(Msg1, MonitorInstance),
  false = monitor:is_role_reachable("B", MonitorInstance1),
  true = monitor:is_role_reachable("C", MonitorInstance1),
  true = monitor:is_role_reachable("D", MonitorInstance1),

  % Next, send Z1() to C: only D should be reachable after
  Msg2 = message:message(0, "A", ["C"], "Z1", [], []),
  {ok, MonitorInstance2} = monitor:send(Msg2, MonitorInstance1),
  false = monitor:is_role_reachable("B", MonitorInstance2),
  false= monitor:is_role_reachable("C", MonitorInstance2),
  true = monitor:is_role_reachable("D", MonitorInstance2),

  % Finally, send Z to D: no roles should be reachable.
  Msg3 = message:message(0, "A", ["D"], "Z", [], []),
  {ok, MonitorInstance3} = monitor:send(Msg3, MonitorInstance2),
  false = monitor:is_role_reachable("B", MonitorInstance3),
  false = monitor:is_role_reachable("C", MonitorInstance3),
  false = monitor:is_role_reachable("D", MonitorInstance3).



