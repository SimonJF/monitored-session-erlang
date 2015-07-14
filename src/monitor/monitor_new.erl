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
                    transitions=Transitions,
                    reachability_dict=orddict:new()}. %% FIXME: Reachability dict for nested FSMs? Union


% Creates a monitor instance given a protocol name, role name,
% and state and transition tables
create_monitor_instance(ProtocolName, RoleName, NestedFSMs) ->
  RootFSM = orddict:fetch(0, NestedFSMs),
  RootFSMInstance = instantiate_monitor(0,
                                        RootFSM#monitor_gen_state.states,
                                        RootFSM#monitor_gen_state.transitions),
  OuterMonitorInstances = orddict:store(0, RootFSMInstance, orddict:new()),
  #outer_monitor_instance{protocol_name = ProtocolName,
                          role_name = RoleName,
                          monitors = NestedFSMs,
                          monitor_instances = OuterMonitorInstances}.



% Check if all nested FSMs are ended. Returns true if yes, false if no.
check_nested_fsms_ended(NestedFSMIDs, MonitorInstance) ->
  NestedFSMs = MonitorInstance#outer_monitor_instance.monitor_instances,
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

get_state(StateNum, MonitorInstance) ->
  StateTable = MonitorInstance#monitor_instance.states,
  orddict:fetch(StateNum, StateTable).

current_fsm_state(FSM) ->
  CurrentStateNum = FSM#monitor_instance.current_state,
  get_state(CurrentStateNum, FSM).

get_fsm(FSMID, MonitorInstance) ->
  orddict:fetch(FSMID, MonitorInstance#outer_monitor_instance.monitor_instances).


get_transitions(FSM) ->
  CurrentState = FSM#monitor_instance.current_state,
  TransitionTable = FSM#monitor_instance.transitions,
  sets:to_list(orddict:fetch(CurrentState, TransitionTable)).

check_receive(Message, Sender, MessageName, _PayloadTypes) ->
  CorrectSender = message:message_sender(Message) == Sender,
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = true, % For now. The "check the strings" thing is nonsense,
                              % I'd much rather do a "type checker" soon.
  %message:message_payload_types(Message) == PayloadTypes,
  case {CorrectSender, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> true;
    {false, _, _} -> {false, bad_sender};
    {_, false, _} -> {false, bad_message_name}
    % {_, _, false} -> {false, bad_payload_types}
  end.

check_send(Message, Recipients, MessageName, PayloadTypes) ->
  CorrectRecipients = lists:sort(Recipients) == lists:sort(message:message_recipients(Message)),
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = true, %message:message_payload_types(Message) == PayloadTypes,
  case {CorrectRecipients, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> true;
    {false, _, _} -> {false, bad_recipients};
    {_, false, _} -> {false, bad_message_name}
    % {_, _, false} -> {false, bad_payload_types}
  end.

transition_kind(send) -> send;
transition_kind(send_call_request) -> send;
transition_kind(send_call_response) -> send;
transition_kind(recv) -> recv;
transition_kind(recv_call_request) -> recv;
transition_kind(recv_call_response) -> recv.


check_transitions([], _, _) -> {error, bad_message};
check_transitions([T|TS], InteractionType, Message) ->
  TransitionType = element(1, T),
  if TransitionType == InteractionType ->
       case transition_kind(TransitionType) of
         send -> check_send(Message

update_monitor(FSM, MonitorInstance) ->
  MonitorInstances = MonitorInstance#outer_monitor_instance.monitor_instances,
  FSMID = FSM#monitor_instance.fsm_id,
  MonitorInstances1 = orddict:store(FSMID, FSM, MonitorInstances),
  MonitorInstance#outer_monitor_instance{monitor_instances=MonitorInstances1}.

get_next_state(InteractionType, Message, FSM, MonitorInstance) ->
  Transitions = get_transitions(FSM),
  TransitionRes = check_transitions(Transitions, InteractionType, Message),
  case TransitionRes of
    {ok, NextID} -> {ok, NextID, MonitorInstance};
    Other -> Other
  end.

% Checks to see whether the given message is accepted by the FSM
% with the given ID.
check_message_in(InteractionType, Message, FSMID, MonitorInstance) ->
  FSM = get_fsm(FSMID, MonitorInstance),
  MonitorRes = get_next_state(InteractionType, Message, FSM,
                              MonitorInstance),
  case MonitorRes of
    {ok, NextID, MonitorInstance1} ->
      FSM1 = FSM#monitor_instance{current_state=NextID},
      {ok, update_monitor(FSM1, MonitorInstance1)};
    Other -> Other
  end.



check_message(InteractionType, Message, OuterMonitorInstance) ->
  % Check from the root FSM.
  RootFSM = orddict:fetch(0, OuterMonitorInstance#outer_monitor_instance.monitor_instances),
  check_message_in(InteractionType, Message, 0, RootFSM, OuterMonitorInstance).

%%%%%%%
%%% API
%%%%%%%

