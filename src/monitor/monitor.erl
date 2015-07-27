-module(monitor).
-compile(export_all).
-include("monitor_records.hrl").

%%%%%%%
%%% Internal Stuffs
%%%%%%%
create_monitor(ScribbleFile, ProtocolName, RoleName) ->
  ParseResult = scribble_lexer:parse(ScribbleFile),
  case ParseResult of
    {ok, AST} ->
      MonitorResult = monitor_gen:generate_monitor_for(AST, ProtocolName, RoleName),
      case MonitorResult of
        {ok, NestedFSMs} ->
          {ok, create_monitor_instance(ProtocolName, RoleName, NestedFSMs)};
        Other -> Other
      end;
    Other -> Other
  end.

% Creates a monitor instance for a local protocol AST
create_monitor(LocalProtocolAST = {local_protocol, ProtocolName, RoleName, _, _, _}) ->
  MonitorResult = monitor_gen:generate_monitor(LocalProtocolAST),
  case MonitorResult of
    {ok, NestedFSMs} ->
      {ok, create_monitor_instance(ProtocolName, RoleName, NestedFSMs)};
    Other -> Other
  end.

instantiate_monitor(FSMID, States, Transitions) ->
  #monitor_instance{fsm_id=FSMID,
                    current_state=0,
                    states=States,
                    transitions=Transitions}.

generate_reachability_dicts([]) -> [];
generate_reachability_dicts([{FSMID, FSM}|XS]) ->
  TempMonitor = instantiate_monitor(FSMID, FSM#monitor_gen_state.states,
                                    FSM#monitor_gen_state.transitions),
  ReachabilityDict = generate_reachability_dict(TempMonitor),
  [{FSMID, ReachabilityDict}|generate_reachability_dicts(XS)].


% Creates a monitor instance given a protocol name, role name,
% and state and transition tables
create_monitor_instance(ProtocolName, RoleName, NestedFSMs) ->
  RootFSM = orddict:fetch(0, NestedFSMs),
  RootFSMInstance = instantiate_monitor(0,
                                        RootFSM#monitor_gen_state.states,
                                        RootFSM#monitor_gen_state.transitions),
  FirstState = orddict:fetch(0, NestedFSMs),
  OuterMonitorInstances = orddict:store(0, RootFSMInstance, orddict:new()),
  ReachabilityDicts = generate_reachability_dicts(orddict:to_list(NestedFSMs)),
  MonitorInstance = #outer_monitor_instance{protocol_name = ProtocolName,
                                            role_name = RoleName,
                                            monitors = NestedFSMs,
                                            monitor_instances = OuterMonitorInstances,
                                            reachability_dicts = ReachabilityDicts},
  case FirstState of
    {ok, {par_node, FSMIDs}} ->
      instantiate_nested_fsms(FSMIDs, MonitorInstance);
    _ -> MonitorInstance
  end.


% Check if all nested FSMs are ended. Returns true if yes, false if no.
check_nested_fsms_ended(NestedFSMIDs, MonitorInstance) ->
  NestedFSMs = MonitorInstance#outer_monitor_instance.monitor_instances,
  %io:format("IDs: ~p, NestedFSMs: ~p~n", [NestedFSMIDs, NestedFSMs]),
  lists:all(fun(ID) ->
                case orddict:find(ID, NestedFSMs) of
                  {ok, FSMState} -> is_fsm_ended(FSMState);
                  _ -> false
                end end, NestedFSMIDs).

new_monitor_instance(MonitorID, MonitorInstance) ->
  MonitorSpecs = MonitorInstance#outer_monitor_instance.monitors,
  MonitorSpec = orddict:fetch(MonitorID, MonitorSpecs),
  instantiate_monitor(MonitorID,
                      MonitorSpec#monitor_gen_state.states,
                      MonitorSpec#monitor_gen_state.transitions).

get_root_fsm(OuterMonitorInstance) ->
  orddict:fetch(0, OuterMonitorInstance#outer_monitor_instance.monitor_instances).

is_ended(MonitorInstance) ->
  is_fsm_ended(get_root_fsm(MonitorInstance)).

is_fsm_ended(FSMState) ->
  CurrentMonitorState = current_fsm_state(FSMState),
  case CurrentMonitorState of
    end_state -> true;
    _Other -> false
  end.

get_state(StateNum, FSM) ->
  StateTable = FSM#monitor_instance.states,
  orddict:fetch(StateNum, StateTable).

current_fsm_state(FSM) ->
  CurrentStateNum = FSM#monitor_instance.current_state,
  get_state(CurrentStateNum, FSM).

current_fsm_state_id(FSM) -> FSM#monitor_instance.current_state.

get_fsm(FSMID, MonitorInstance) ->
  orddict:fetch(FSMID, MonitorInstance#outer_monitor_instance.monitor_instances).

get_transitions_from(NodeID, FSM) ->
  TransitionTable = FSM#monitor_instance.transitions,
  case orddict:find(NodeID, TransitionTable) of
    {ok, TransitionSet} -> sets:to_list(TransitionSet);
    _ -> []
  end.

get_transitions(FSM) ->
  CurrentState = FSM#monitor_instance.current_state,
  get_transitions_from(CurrentState, FSM).

check_receive(Message, RecvTransition)->
  {_, _, Sender, MessageName, _Payloads} = RecvTransition,
  CorrectSender = message:message_sender(Message) == Sender,
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = true, % For now. The "check the strings" thing is nonsense,
                              % I'd much rather do a "type checker" soon.
  %message:message_payload_types(Message) == PayloadTypes,
  case {CorrectSender, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> true;
    _ -> false
    % {false, _, _} -> {false, bad_sender};
    % {_, false, _} -> {false, bad_message_name}
    % {_, _, false} -> {false, bad_payload_types}
  end.

check_send(Message, SendTransition) ->
  {_, _, Recipients, MessageName, _Payloads} = SendTransition,
  CorrectRecipients = lists:sort(Recipients) == lists:sort(message:message_recipients(Message)),
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = true, %message:message_payload_types(Message) == PayloadTypes,
  case {CorrectRecipients, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> true;
    _ -> false
    %{false, _, _} -> {false, bad_recipients};
    %{_, false, _} -> {false, bad_message_name}
    % {_, _, false} -> {false, bad_payload_types}
  end.

transition_kind(send) -> send;
transition_kind(send_call_request) -> send;
transition_kind(send_call_response) -> send;
transition_kind(recv) -> recv;
transition_kind(recv_call_request) -> recv;
transition_kind(recv_call_response) -> recv.


instantiate_nested_fsms([], MonitorInstance) ->
  MonitorInstance;
instantiate_nested_fsms([FSMID|FSMIDs], MonitorInstance) ->
  Monitor = new_monitor_instance(FSMID, MonitorInstance),
  MonitorInstanceDict = MonitorInstance#outer_monitor_instance.monitor_instances,
  MonitorInstanceDict1 = orddict:store(FSMID, Monitor, MonitorInstanceDict),

  MonitorInstance1 =
    MonitorInstance#outer_monitor_instance{monitor_instances=MonitorInstanceDict1},
  instantiate_nested_fsms(FSMIDs, MonitorInstance1).

check_nested_fsms([], _, _) ->
  {error, no_nested_fsm_match};
check_nested_fsms(IDList = [NestedID|_], Action, MonitorInstance) ->
  % First, check whether the FSM exists (has been instantiated) -- if not, instantiate it
  MonitorInstanceDict = MonitorInstance#outer_monitor_instance.monitor_instances,
  KeyExists = orddict:is_key(NestedID, MonitorInstanceDict),
  MonitorInstance1 =
    if not KeyExists -> instantiate_nested_fsms(IDList, MonitorInstance);
       KeyExists -> MonitorInstance
    end,
  check_nested_fsms_inner(IDList, Action, MonitorInstance1).

check_nested_fsms_inner([], _, _) ->
  {error, no_nested_fsm_match};
check_nested_fsms_inner([NestedID|IDs], Action, MonitorInstance) ->
  CheckRes = check_action_in(Action, NestedID, MonitorInstance),
  % Go with the first match. If there are no matches, reject the message.
  case CheckRes of
    {ok, MonitorInstance1} -> {ok, MonitorInstance1};
    _ ->
      % If not, check the next one
      check_nested_fsms(IDs, Action, MonitorInstance)
  end.

% Checks for the first possible transition (MSAs are deterministic).
% Returns either {error, Err} or
% {ok, NextStateID, NewMonitorInstance, bool describing whether or not the
%   transition happened in a nested FSM}
check_transitions([], _, _, _) -> {error, bad_message};
check_transitions([Transition = {par_transition, _NextStateID, NestedFSMIDs}|TS],
                  Action, FSMState, Monitor) ->
  CheckRes = check_nested_fsms(NestedFSMIDs, Action, Monitor),
  case CheckRes of
    {ok, NewMonitor} ->
      % Need to distinguish between staying on the same state and updating internally,
      % and staying on the same state because of an external self-transition.
      % At this point, we know a transition's been taken.
      % Check whether the nested FSMs are all finished...
      FSMsFinished = check_nested_fsms_ended(NestedFSMIDs, NewMonitor),
      {ok, Transition, NewMonitor, not FSMsFinished};
    _ -> check_transitions(TS, Action, FSMState, Monitor)
  end;
check_transitions([T|TS], Action={message, {InteractionType, Message}}, FSMState, Monitor) ->
  TransitionType = element(1, T),
  if TransitionType == InteractionType ->
       CanTakeTransition =
         case transition_kind(TransitionType) of
           send -> check_send(Message, T);
           recv -> check_receive(Message, T)
         end,
         if CanTakeTransition -> {ok, T, Monitor, false};
            not CanTakeTransition ->
              % Right transition type, wrong parameters
              check_transitions(TS, Action, FSMState, Monitor)
         end;
     true ->
       % Transition is of the wrong type
       check_transitions(TS, Action, FSMState, Monitor)
  end;
check_transitions([T|TS], Action={subsession_action, SubsessionAction}, FSMState, Monitor) ->
  TransitionType = element(1, T),
  SubsessionActionType = element(1, SubsessionAction),
  if SubsessionActionType == TransitionType ->
       case SubsessionAction of
         {start_subsession, SubsessionName, InternalInvites, ExternalInvites} ->
           CheckStartRes =
             check_start_subsession(T, SubsessionName, InternalInvites, ExternalInvites),
           if CheckStartRes ->
                {ok, T, Monitor, false};
              not CheckStartRes ->
                check_transitions(TS, Action, FSMState, Monitor)
           end;
         {subsession_success} -> {ok, T, Monitor, false};
         {subsession_failure, FailureName} ->
           TransitionFailureName = element(3, T),
           if FailureName == TransitionFailureName ->
                {ok, T, Monitor, false};
              FailureName =/= TransitionFailureName ->
                % Wrong handle block
                check_transitions(TS, Action, FSMState, Monitor)
           end
       end;
     SubsessionActionType =/= TransitionType ->
       % Wrong transition type
       check_transitions(TS, Action, FSMState, Monitor)
  end.


check_start_subsession({start_subsession, _, TSubsessionName, TExternal, TInternal},
                       SubsessionName, Internal, External) ->
  InternalRes = check_lists(TInternal, Internal),
  % Forwards-compatibility here -- allowing both {Name, PID} and Name
  ExternalList = lists:map(fun(X) -> case X of {Name, _} -> Name;
                                               Name -> Name
                                     end end, External),
  ExternalRes = check_lists(TExternal, ExternalList),
  NameRes = (TSubsessionName == SubsessionName),
  NameRes andalso InternalRes andalso ExternalRes.

check_lists(TList, List) ->
  lists:sort(TList) == lists:sort(List).

delete_monitor_instances(FSMIDs, MonitorInstance) ->
  MonitorInstanceDict = MonitorInstance#outer_monitor_instance.monitor_instances,
  MonitorInstanceDict1 =
    lists:foldr(fun(X, Acc) -> orddict:erase(X, Acc) end, MonitorInstanceDict, FSMIDs),
  MonitorInstance#outer_monitor_instance{monitor_instances=MonitorInstanceDict1}.

make_transition(FSM, Transition, IsNestedTransition, MonitorInstance) ->
  %CurrentID = FSM#monitor_instance.current_state,
  TransitionType = element(1, Transition),
  TransitionID = element(2, Transition),
  CurrentID = current_fsm_state_id(FSM),
  NextID =
    if IsNestedTransition -> CurrentID;
       not IsNestedTransition -> TransitionID
    end,
  FSM1 = FSM#monitor_instance{current_state=NextID},

  % IsNestedTransition: Was the transition on an internal (non-root) FSM?
  % If yes -- nothing doing.
  % If no, and we just took a par transition, remove the FSM IDs from
  % the monitor instance list.
  if ((TransitionType =/= par_transition) or IsNestedTransition) ->
       update_monitor(FSM1, MonitorInstance);
     true ->
       % Delete monitor instances
       NestedFSMIDs = element(3, Transition),
       MonitorInstance1 = delete_monitor_instances(NestedFSMIDs, MonitorInstance),
       update_monitor(FSM1, MonitorInstance1)
  end.

update_monitor(FSM, MonitorInstance) ->
  MonitorInstances = MonitorInstance#outer_monitor_instance.monitor_instances,
  FSMID = FSM#monitor_instance.fsm_id,
  MonitorInstances1 = orddict:store(FSMID, FSM, MonitorInstances),
  MonitorInstance#outer_monitor_instance{monitor_instances=MonitorInstances1}.

get_next_state(Action, FSM, MonitorInstance) ->
  Transitions = get_transitions(FSM),
  TransitionRes = check_transitions(Transitions, Action,
                                    FSM, MonitorInstance),
  case TransitionRes of
    {ok, Transition, NewMonitorInstance, IsNestedTransition} ->
      {ok, Transition, NewMonitorInstance, IsNestedTransition};
    Other -> Other
  end.

% Check whether
% A) We're on a par node
% and if so
% B) if all nested FSMs are finished.
% Returns {true, ID list} if an unfinished par node
% false if not.
is_on_par_node(FSM, MonitorInstance) ->
  State = current_fsm_state(FSM),
  case State of
    {par_state, IDs} ->
      NestedFSMsFinished = check_nested_fsms_ended(IDs, MonitorInstance),
      if not NestedFSMsFinished -> {true, IDs};
         NestedFSMsFinished -> false
      end;
    _ -> false
  end.


% Checks to see whether the given action is accepted by the FSM
% with the given ID.
check_action_in(Action, FSMID, MonitorInstance) ->
  FSM = get_fsm(FSMID, MonitorInstance),
  MonitorRes = get_next_state(Action, FSM, MonitorInstance),
  case MonitorRes of
    {ok, Transition, MonitorInstance1, IsNestedTransition} ->
      MonitorInstance2 = make_transition(FSM, Transition, IsNestedTransition, MonitorInstance1),
      {ok, MonitorInstance2};
    Other -> Other
  end.

check_action(Action, MonitorInstance) ->
  % Check from the root FSM.
  check_action_in(Action, 0, MonitorInstance).


%%%%%%%
%%% Reachability Stuff
%%%%%%%

roles_fsms_involved({par_transition, _, FSMIDs}) ->
  {[], FSMIDs};
roles_fsms_involved({start_subsession, _, _, IRs, _}) -> {IRs, []};
roles_fsms_involved({subsession_success, _}) -> {[], []};
roles_fsms_involved({subsession_failure, _, _}) -> {[], []};
roles_fsms_involved(T) ->
  InteractionType = element(1, T),
  RoleRes = element(3, T),
  IsList = (transition_kind(InteractionType) == send),
  if IsList ->
       {RoleRes, []};
     not IsList ->
       {[RoleRes], []}
  end.


add_transition_info(T, FSM, CurrentPathRoles, CurrentPathFSMIDs,
                     ReachableDict, VisitedSet) ->
  {TransitionRoleList, TransitionFSMIDsList} = roles_fsms_involved(T),
  TransitionRoles = sets:from_list(TransitionRoleList),
  TransitionFSMIDs = sets:from_list(TransitionFSMIDsList),
  ToID = element(2, T),
  NewCurrentPathRoles = sets:union(TransitionRoles, CurrentPathRoles),
  NewCurrentPathFSMIDs = sets:union(TransitionFSMIDs, CurrentPathFSMIDs),
  {SubpathRoles, SubpathFSMIDs, NewDict} =
    reachable_from_inner(ToID, FSM, NewCurrentPathRoles, NewCurrentPathFSMIDs,
                         ReachableDict, VisitedSet),
  {sets:union(SubpathRoles, TransitionRoles),
   sets:union(SubpathFSMIDs, TransitionFSMIDs),
   NewDict}.


% NodeID: Current node ID
% FSM: Current FSM
% Monitor: Current monitor instance
% CurrentReachableSet: Roles involved in this current path -- used if we revisit a node
% ReachableDict: NodeID |-> [ReachableRole]
% VisitedSet: Set of visited nodes
reachable_from_inner(NodeID, FSM, CurrentPathRoles, CurrentPathFSMIDs,
                     ReachableDict, VisitedSet) ->
  % First: check whether the node has already been visited.
  % If so, return the current reachable set.
  case sets:is_element(NodeID, VisitedSet) of
    true -> {CurrentPathRoles, CurrentPathFSMIDs, ReachableDict};
    false ->
      % If not, we need to check each of the outgoing transitions for
      % the roles involved in each path.
      % Roles involved in a path = Transition roles + roles in subpath.
      % Roles involved in this node = Union of all path roles.
      NewVisitedSet = sets:add_element(NodeID, VisitedSet),
      Transitions = get_transitions_from(NodeID, FSM),
      {PathRoleUnion, PathFSMIDUnion, ReachableDict1} =
        lists:foldr(fun(T, {WorkingRoleSet, WorkingIDSet, WorkingDict}) ->
                        {SubpathRoles, SubpathFSMIDs, NewDict} =
                          add_transition_info(T, FSM,
                                              CurrentPathRoles, CurrentPathFSMIDs,
                                              WorkingDict, NewVisitedSet),
                          NewWorkingRoleSet = sets:union(SubpathRoles, WorkingRoleSet),
                          NewWorkingIDSet = sets:union(SubpathFSMIDs, WorkingIDSet),
                          {NewWorkingRoleSet, NewWorkingIDSet, NewDict}
                    end,
                    {sets:new(), sets:new(), ReachableDict}, Transitions),
      % Now we have all path roles, we can store into the dict.
      ReachableDict2 = orddict:store(NodeID, {PathRoleUnion, PathFSMIDUnion}, ReachableDict1),
      {PathRoleUnion, PathFSMIDUnion, ReachableDict2}
  end.

generate_reachability_dict(FSM) ->
  {_, _, Res} = reachable_from_inner(0, FSM, sets:new(), sets:new(),
                                  orddict:new(), sets:new()),
  Res.

get_reachability_dict(FSMID, Monitor) ->
  ReachabilityDicts = Monitor#outer_monitor_instance.reachability_dicts,
  orddict:fetch(FSMID, ReachabilityDicts).

is_role_reachable(RoleName, Monitor) ->
  FSM = get_fsm(0, Monitor),
  CurrentState = current_fsm_state_id(FSM),
  is_role_reachable_inner(RoleName, 0, CurrentState, Monitor).

is_role_reachable_inner(RoleName, FSMID, CurrentState, Monitor) ->
  ReachabilityDict = get_reachability_dict(FSMID, Monitor),
  {ReachableRoles, InvolvedFSMs} = orddict:fetch(CurrentState, ReachabilityDict),
  IsInvolved = sets:is_element(RoleName, ReachableRoles),
  InvolvedFSMsList = sets:to_list(InvolvedFSMs),
  IsInvolvedInNested =
    lists:any(fun(NestedFSMID) ->
                  check_nested_fsm_reachability(RoleName, NestedFSMID, Monitor) end,
              InvolvedFSMsList),
  IsInvolved or IsInvolvedInNested.

check_nested_fsm_reachability(RoleName, FSMID, Monitor) ->
  MonitorInstances = Monitor#outer_monitor_instance.monitor_instances,
  % First, check if the monitor's instantiated. If so, get the current
  % state. If not, then it's 0.
  NestedFSMStateID =
    case orddict:find(FSMID, MonitorInstances) of
      {ok, FSM} -> FSM#monitor_instance.current_state;
      error -> 0
    end,
  % Alright, now, check to see if the role's reachable in the
  % nested FSM
  is_role_reachable_inner(RoleName, FSMID, NestedFSMStateID, Monitor).

%%%%%%%
%%% API
%%%%%%%

% Action:
  % {subsession_action, {start, session name, internal invites, external invites}}
  % {subsession_action, {success}}
  % {subsession_action, {failure, failure name}}
  % {message, {interaction type, message}}

% Checks whether a given message can be sent, given the current monitor state,
% and advance the monitor state
send(Message, MonitorInstance) ->
  check_action({message, {send, Message}}, MonitorInstance).

% Checks whether a given message can be received, given the current monitor state,
% and advance the monitor state
recv(Message, MonitorInstance) ->
  check_action({message, {recv, Message}}, MonitorInstance).

send_call_request(Message, MonitorInstance) ->
  check_action({message, {send_call_req, Message}}, MonitorInstance).
recv_call_request(Message, MonitorInstance) ->
  check_action({message, {recv_call_req, Message}}, MonitorInstance).
send_call_response(Message, MonitorInstance) ->
  check_action({message, {send_call_resp, Message}}, MonitorInstance).
recv_call_response(Message, MonitorInstance) ->
  check_action({message, {recv_call_resp, Message}}, MonitorInstance).

start_subsession(SubsessionName, InternalInvitations,
                 ExternalInvitations, MonitorInstance) ->
  check_action({subsession_action, {start_subsession, SubsessionName,
                                    InternalInvitations,
                                    ExternalInvitations}}, MonitorInstance).

subsession_success(MonitorInstance) ->
  check_action({subsession_action, {subsession_success}}, MonitorInstance).

subsession_failure(FailureName, MonitorInstance) ->
  check_action({subsession_action, {subsession_failure, FailureName}},
              MonitorInstance).
