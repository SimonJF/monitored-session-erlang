-module(monitor).
-compile(export_all).

% monitor.erl: Functions for manipulating monitors.
% The monitor runtime, and some public-facing API functions to load them in.

% monitor_instance: A monitor instance for a local protocol.
% Specifies the protocol name, the role to which the protocol has been
% projected, the current state in the monitor, and the state and transition
% tables.
-record(monitor_instance, {protocol_name,
                           role_name,
                           current_state = 0,
                           states,
                           transitions}).

% Public-facing API to create a monitor instance for the given Scribble
% file, protocol name, and role name.
create_monitor(ScribbleFile, ProtocolName, RoleName) ->
  ParseResult = scribble_lexer:parse(ScribbleFile),
  case ParseResult of
    {ok, AST} ->
      MonitorResult = monitor_gen:generate_monitor_for(AST, ProtocolName, RoleName),
      case MonitorResult of
        {ok, {_RID, States, Transitions}} ->
          {ok, create_monitor_instance(ProtocolName, RoleName, States, Transitions)};
        Other -> Other
      end;
    Other -> Other
  end.

% Creates a monitor instance given a protocol name, role name,
% and state and transition tables
create_monitor_instance(ProtocolName, RoleName, States, Transitions) ->
  #monitor_instance{protocol_name = ProtocolName,
                    role_name = RoleName,
                    states = States,
                    transitions = Transitions}.

% Returns the current monitor node
current_monitor_node(MonitorInstance) ->
  CurrentStateNum = MonitorInstance#monitor_instance.current_state,
  get_state(CurrentStateNum, MonitorInstance).

% Returns a list of outgoing nodes from the given node
outgoing_transitions(MonitorNode = {_NodeType, Id, _Info}, MonitorInstance) ->
  Transitions = MonitorInstance#monitor_instance.transitions,
  % If a state has no outgoing transitions, it may not be in the transition table.
  % That's fine -- just return the empty list.
  FindResult = orddict:find(Id, Transitions),
  case FindResult of
    {ok, TransitionSet} ->
      TransitionList = sets:to_list(TransitionSet),
      lists:map(fun(TransitionID) ->
                    get_state(TransitionID, MonitorInstance) end, TransitionList);
    error -> []
  end.

% Gets a monitor node given a state number
get_state(StateNum, MonitorInstance) ->
  StateTable = MonitorInstance#monitor_instance.states,
  orddict:fetch(StateNum, StateTable).

% Transition lists should be singletons.
% An empty list means that there's no available transition for the given message
% (meaning the message is invalid), or that there's possible nondeterminism:
% nondeterminism is forbidden by global protocol well-formedness conditions
check_transition_list(TransitionList) ->
  case TransitionList of
    [] -> {error, no_transitions};
    [X] -> {ok, X};
    [X|XS] -> {error, nondeterminism}
  end.

% These utility functions are a tad on the messy side...
transition_next_node(Message, MonitorNode, PredicateResult, MonitorInstance) ->
  case PredicateResult of
    true ->
      % Check to ensure there's exactly one outward transition, otherwise
      % something's wrong.
      check_transition_list(outgoing_transitions(MonitorNode, MonitorInstance));
    {false, Error} -> {error, Error}
  end.

% Used when checking choice blocks.
% InteractionType is the type of interaction -- so send or receive
% NodeType is the node type (send_node, receive_node, or choice_node in this case)
% Message is the message to be sent
% MonitorNode is the current monitor node (probably a choice node)
% PredicateFunction is the function to check a message
% MonitorInstance is the current monitor instance
filter_outgoing_transitions(InteractionType, NodeType, Message, MonitorNode, PredicateFunction, MonitorInstance) ->
  % Check whether each possible outgoing transition is actually one we could take
  lists:filtermap(fun(Node) ->
      if element(1, (Node)) == NodeType ->
           case PredicateFunction(Message, Node, MonitorInstance) of
             true ->
               NNRes = next_node(InteractionType, Message, Node, MonitorInstance),
               case NNRes of
                 {ok, NN} -> {true, NN};
                 _Other -> false
               end;
             {false, Error} -> false
           end;
         % If not, don't include
         % Erlang is stupid
         true -> false
      end
    end, outgoing_transitions(MonitorNode, MonitorInstance)).

% Gets the next node, given an interaction type, message, monitor node, and monitor instance.
% This will either be {ok, Node} or {error , Error}
next_node(InteractionType, Message, MonitorNode = {choice_node, Id, _Info}, MonitorInstance) ->
  % - In case of sends, filter send nodes; converse for receive nodes
  % - Filter valid ones
  %
  % filtermap(Fun, List1) -> List2
  %Fun = fun((Elem) -> boolean() | {true, Value})
  %List1 = [Elem]
  %List2 = [Elem | Value]
  %Elem = Value = term()
  {NodeType, PredicateFunction} =
    case InteractionType of
      send -> {send_node, fun monitor:can_send_at/3};
      recv -> {receive_node, fun monitor:can_receive_at/3}
    end,
  FilteredTransitions = filter_outgoing_transitions(InteractionType,
                                                    NodeType,
                                                    Message,
                                                    MonitorNode,
                                                    PredicateFunction,
                                                    MonitorInstance),
  check_transition_list(FilteredTransitions);
next_node(recv, Message, MonitorNode, MonitorInstance) ->
  transition_next_node(Message,
                       MonitorNode,
                       can_receive_at(Message, MonitorNode, MonitorInstance),
                       MonitorInstance);
next_node(send, Message, MonitorNode, MonitorInstance) ->
  transition_next_node(Message,
                       MonitorNode,
                       can_send_at(Message, MonitorNode, MonitorInstance),
                       MonitorInstance);
next_node(_IT, _Msg, _MN, _MI) ->
  {error, bad_node}.

% Checks whether we can send or receive at this point
can_receive_at(Message, MonitorNode = {receive_node, Id, Info}, MonitorInstance) ->
  {Sender, MessageName, PayloadTypes} = Info,
  CorrectSender = message:message_sender(Message) == Sender,
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = message:message_payload_types(Message) == PayloadTypes,
  case {CorrectSender, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> true;
    {false, _, _} -> {false, bad_sender};
    {_, false, _} -> {false, bad_message_name};
    {_, _, false} -> {false, bad_payload_types}
  end;
can_receive_at(Message, MonitorNode = {choice_node, Id, _Info}, MonitorInstance) ->
  % Righto. At this point, we need to check all of the outgoing transitions to
  % see whether they have an appropriate receive output node.
  % If so, we take the transition out of that.
  OutgoingTransitions = filter_outgoing_transitions(recv,
                                                    receive_node,
                                                    Message,
                                                    MonitorNode,
                                                    fun monitor_gen:can_receive_at/3,
                                                    MonitorInstance),
  lists:length(OutgoingTransitions) == 1;
can_receive_at(_Message, MonitorNode, _MonitorInstance) ->
  {false, bad_node_type}.


can_send_at(Message, MonitorNode = {send_node, Id, Info}, MonitorInstance) ->
  io:format("can_send_at called, node: ~p~n", [MonitorNode]),
  {Recipients, MessageName, PayloadTypes} = Info,
  CorrectRecipients = lists:sort(Recipients) == lists:sort(message:message_recipients(Message)),
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = message:message_payload_types(Message) == PayloadTypes,
  case {CorrectRecipients, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> true;
    {false, _, _} -> {false, bad_recipients};
    {_, false, _} -> {false, bad_message_name};
    {_, _, false} -> {false, bad_payload_types}
  end;
can_send_at(_Message, MonitorNode, MonitorInstance) ->
  io:format("can_send_at called, node: ~p~n", [MonitorNode]),
  {false, bad_node_type}.


% Checks a message given an interaction type, message, and monitor instance
check_message(InteractionType, Message, MonitorInstance) ->
  % Aight. Firstly, get the current node. The next thing to do is try and get the next node,
  % and if that works, then update the monitor state.
  CurrentNode = current_monitor_node(MonitorInstance),
  NextNodeResult = next_node(InteractionType, Message, CurrentNode, MonitorInstance),
  case NextNodeResult of
    {ok, NextNode = {_NodeType, Id, _Info}} ->
      NewMonitorInstance = MonitorInstance#monitor_instance{current_state=Id},
      {ok, NewMonitorInstance};
    {error, Error} -> {error, Error, MonitorInstance}
  end.

is_ended(MonitorInstance) ->
  CurrentMonitorNode = current_monitor_node(MonitorInstance),
  case CurrentMonitorNode of
    {end_node, _, _} -> true;
    _Other -> false
  end.

% Checks whether a given message can be sent, given the current monitor state,
% and advance the monitor state
send(Message, MonitorInstance) ->
  check_message(send, Message, MonitorInstance).

% Checks whether a given message can be received, given the current monitor state,
% and advance the monitor state
recv(Message, MonitorInstance) ->
  check_message(recv, Message, MonitorInstance).
