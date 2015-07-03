-module(monitor).
-compile(export_all).
-include("monitor_records.hrl").

% monitor.erl: Functions for manipulating monitors.
% The monitor runtime, and some public-facing API functions to load them in.

% monitor_instance: A monitor instance for a local protocol.
% Specifies the protocol name, the role to which the protocol has been
% projected, the current state in the monitor, and the state and transition
% tables


% Public-facing API to create a monitor instance for the given Scribble
% file, protocol name, and role name.
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

instantiate_monitor(States, Transitions) ->
  #monitor_instance{current_state=0,
                    states=States,
                    transitions=Transitions,
                    reachability_dict=orddict:new()}. %% FIXME: Reachability dict for nested FSMs? Union


% Creates a monitor instance given a protocol name, role name,
% and state and transition tables
create_monitor_instance(ProtocolName, RoleName, NestedFSMs) ->
  RootFSM = orddict:fetch(0, NestedFSMs),
  RootFSMInstance = instantiate_monitor(RootFSM#monitor_gen_state.states,
                                        RootFSM#monitor_gen_state.transitions),
  OuterMonitorInstances = orddict:store(0, RootFSMInstance, orddict:new()),
  #outer_monitor_instance{protocol_name = ProtocolName,
                          role_name = RoleName,
                          monitors = NestedFSMs,
                          monitor_instances = OuterMonitorInstances}.


% Returns the current monitor node
current_monitor_node(MonitorInstance) ->
  CurrentStateNum = MonitorInstance#monitor_instance.current_state,
  get_state(CurrentStateNum, MonitorInstance).

% Returns a list of outgoing nodes from the given node
outgoing_transitions({_NodeType, Id, _Info}, OuterMonitorInstance, MonitorInstance) ->
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
    [_X|_XS] -> {error, nondeterminism}
  end.

% These utility functions are a tad on the messy side...
transition_next_node(_Message, MonitorNode, PredicateResult, OuterMonitorInstance,
                    MonitorInstance) ->
  case PredicateResult of
    true ->
      % Check to ensure there's exactly one outward transition, otherwise
      % something's wrong.
      check_transition_list(outgoing_transitions(MonitorNode, OuterMonitorInstance,
                                                MonitorInstance));
    {false, Error} -> {error, Error}
  end.

% Used when checking choice blocks.
% InteractionType is the type of interaction -- so send or receive
% NodeType is the node type (send_node, receive_node, or choice_node in this case)
% Message is the message to be sent
% MonitorNode is the current monitor node (probably a choice node)
% PredicateFunction is the function to check a message
% OuterMonitorInstance is the current monitor instance
filter_outgoing_transitions(InteractionType, NodeType, Message, MonitorNode, PredicateFunction,
                            OuterMonitorInstance, MonitorInstance) ->
  % Check whether each possible outgoing transition is actually one we could take
  lists:filtermap(fun(Node) ->
      NodeTy = element(1, Node),
      if (NodeTy == rec_node) or (NodeTy == choice_node) ->
           NNRes = next_node(InteractionType, Message, Node,
                             OuterMonitorInstance, MonitorInstance),
           case NNRes of
             {ok, NN} -> {true, NN};
             _Other -> false
           end;
         NodeTy == NodeType ->
           case PredicateFunction(Message, Node, MonitorInstance) of
             true ->
               NNRes = next_node(InteractionType, Message, Node, OuterMonitorInstance,
                                MonitorInstance),
               case NNRes of
                 {ok, NN} -> {true, NN};
                 _Other -> false
               end;
             {false, _Error} -> false
           end;
        % If not, don't include
        % Erlang is stupid
        true -> false
      end
    end, outgoing_transitions(MonitorNode, OuterMonitorInstance, MonitorInstance)).

% Gets the next node, given an interaction type, message, monitor node, and monitor instance.
% This will either be {ok, Node} or {error , Error}
next_node(InteractionType, Message, MonitorNode = {NodeTy, _Id, _Info}, OuterMonitorInstance,
          MonitorInstance) when
    ((NodeTy == choice_node) or (NodeTy == rec_node)) ->
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
      recv -> {receive_node, fun monitor:can_receive_at/3};
      send_call_req -> {call_request_send_node, fun monitor:can_send_request_at/3};
      recv_call_req -> {call_request_recv_node, fun monitor:can_receive_request_at/3};
      send_call_resp -> {call_response_send_node, fun monitor:can_send_response_at/3};
      recv_call_resp -> {call_response_recv_node, fun monitor:can_receive_response_at/3}
    end,
  FilteredTransitions = filter_outgoing_transitions(InteractionType,
                                                    NodeType,
                                                    Message,
                                                    MonitorNode,
                                                    PredicateFunction,
                                                    OuterMonitorInstance,
                                                    MonitorInstance
                                                   ),
  check_transition_list(FilteredTransitions);
next_node(InteractionType, Message, MonitorNode, OuterMonitorInstance, MonitorInstance) ->
  MonitorFn =
    case InteractionType of
      send -> fun monitor:can_send_at/3;
      recv -> fun monitor:can_receive_at/3;
      send_call_req -> fun monitor:can_send_request_at/3;
      recv_call_req -> fun monitor:can_receive_request_at/3;
      send_call_resp -> fun monitor:can_send_response_at/3;
      recv_call_resp -> fun monitor:can_receive_response_at/3;
      _Other -> none
    end,
  if MonitorFn =/= none ->
    transition_next_node(Message,
                         MonitorNode,
                         MonitorFn(Message, MonitorNode, MonitorInstance),
                         OuterMonitorInstance,
                         MonitorInstance
                        );
    MonitorFn == none ->
       {error, bad_node}
  end.


% Checks whether we can send or receive at this point
% Checking a call response and a receive have identical logic.
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

can_receive_at(Message, {receive_node, _Id, Info}, _OuterMonitorInstance) ->
  {Sender, MessageName, PayloadTypes} = Info,
  check_receive(Message, Sender, MessageName, PayloadTypes);
can_receive_at(Message, _MonitorNode, _OuterMonitorInstance) ->
  error_logger:error_msg("Monitor rejected message ~p: expected to receive~n",
                        [Message]),
  {false, bad_node_type}.


can_receive_request_at(Message, {call_request_recv_node, _Id, Info}, _OuterMonitorInstance) ->
  {Recipient, MessageName, PayloadTypes} = Info,
  check_receive(Message, Recipient, MessageName, PayloadTypes);
can_receive_request_at(Message, _MonitorNode, _OuterMonitorInstance) ->
  error_logger:error_msg("Monitor rejected message ~p:" ++
                         " expected to receive a call request~n", [Message]),
  {false, bad_node_type}.


can_receive_response_at(Message, {call_response_recv_node, _Id, Info}, _MI) ->
  {Sender, MessageName, PayloadType} = Info,
  check_receive(Message, Sender, MessageName, [PayloadType]);
can_receive_response_at(Message, _MonitorNode, _OuterMonitorInstance) ->
  error_logger:eror_msg("Monitor rejected message ~p: expected to receive" ++
                        "a call response~n", [Message]),
  {false, bad_node_type}.

% Checking a send and a call request share the same logic.
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

can_send_at(Message, {send_node, _Id, Info}, _OuterMonitorInstance) ->
  {Recipients, MessageName, PayloadTypes} = Info,
  check_send(Message, Recipients, MessageName, PayloadTypes);
can_send_at(Message, _MonitorNode, _OuterMonitorInstance) ->
  error_logger:error_msg("Monitor rejected message ~p: tried to send~n", [Message]),
  {false, bad_node_type}.

can_send_request_at(Message, {call_request_send_node, _Id, Info}, _OuterMonitorInstance) ->
  {Recipient, MessageName, PayloadTypes} = Info,
  check_send(Message, [Recipient], MessageName, PayloadTypes);
can_send_request_at(Message, MonitorNode, _OuterMonitorInstance) ->
  error_logger:error_msg("Monitor rejected message ~p:" ++
                         " tried to send a call request; monitor node: ~p~n", [Message, MonitorNode]),
  {false, bad_node_type}.

can_send_response_at(Message, {call_response_send_node, _Id, Info}, _OuterMonitorInstance) ->
  {Recipient, MessageName, PayloadTypes} = Info,
  check_send(Message, [Recipient], MessageName, PayloadTypes);
can_send_response_at(Message, _MonitorNode, _OuterMonitorInstance) ->
  error_logger:error_msg("Monitor rejected message ~p:" ++
                         " tried to send a call response~n", [Message]),
  {false, bad_node_type}.



% Checks a message given an interaction type, message, and monitor instance
check_message(InteractionType, Message, OuterMonitorInstance) ->
  % Aight. Firstly, get the current node. The next thing to do is try and get the next node,
  % and if that works, then update the monitor state.

  RootFSM = orddict:fetch(0, OuterMonitorInstance#outer_monitor_instance.monitor_instances),
  CurrentNode = current_monitor_node(RootFSM),
  NextNodeResult = next_node(InteractionType, Message, CurrentNode, OuterMonitorInstance,
                            RootFSM),
  % TODO: Things will have to change here, I think
  % next_node will need to return a couple of things. Essentially since we'll be
  % dealing with nested FSMs, we will need to extend it to find the appropriate
  % nested FSM, update that, etc.
  % What we're doing atm is just updating the outermost one.
  case NextNodeResult of
    {ok, {_NodeType, Id, _Info}} ->
      io:format("In ok section, new ID: ~p~n", [Id]),

      NewMonitorInstance = RootFSM#monitor_instance{current_state=Id},
      MonitorInstances = OuterMonitorInstance#outer_monitor_instance.monitor_instances,
      NewMonitorInstances = orddict:store(0, NewMonitorInstance, MonitorInstances),
      NewOuterMonitorInstance =
      OuterMonitorInstance#outer_monitor_instance{monitor_instances=NewMonitorInstances},
      {ok, NewOuterMonitorInstance};
    {error, Error} ->
      io:format("In error section~n"),
      {error, Error, OuterMonitorInstance}
  end.

% TODO: this should be per-monitor
is_ended(OuterMonitorInstance) ->
  RootFSM = orddict:fetch(0, OuterMonitorInstance#outer_monitor_instance.monitor_instances),
  CurrentMonitorNode = current_monitor_node(RootFSM),
  case CurrentMonitorNode of
    {end_node, _, _} -> true;
    _Other -> false
  end.

% Checks whether a given message can be sent, given the current monitor state,
% and advance the monitor state
send(Message, OuterMonitorInstance) ->
  check_message(send, Message, OuterMonitorInstance).

% Checks whether a given message can be received, given the current monitor state,
% and advance the monitor state
recv(Message, OuterMonitorInstance) ->
  check_message(recv, Message, OuterMonitorInstance).

send_call_request(Message, OuterMonitorInstance) ->
  check_message(send_call_req, Message, OuterMonitorInstance).
recv_call_request(Message, OuterMonitorInstance) ->
  check_message(recv_call_req, Message, OuterMonitorInstance).
send_call_response(Message, OuterMonitorInstance) ->
  check_message(send_call_resp, Message, OuterMonitorInstance).
recv_call_response(Message, OuterMonitorInstance) ->
  check_message(recv_call_resp, Message, OuterMonitorInstance).

call_response(Message, OuterMonitorInstance) ->
  check_message(call_resp, Message, OuterMonitorInstance).

role_name(OuterMonitorInstance) ->
  OuterMonitorInstance#outer_monitor_instance.role_name.


%% Gets the roles involved at a certain node
roles_involved({NodeType, _, {Recipient,  _, _}})
  when (NodeType == receive_node) or
       (NodeType == call_request_send_node) or
       (NodeType == call_request_recv_node) or
       (NodeType == call_response_send_node) or
       (NodeType == call_response_recv_node) ->
  [Recipient];
roles_involved({NodeType, _, {Recipients,  _, _}})
  when (NodeType == send_node) -> Recipients;
roles_involved(_Other) -> [].

%outgoing_transitions({_NodeType, Id, _Info}, OuterMonitorInstance) ->
%  Transitions = OuterMonitorInstance#monitor_instance.transitions,
  % If a state has no outgoing transitions, it may not be in the transition table.
  % That's fine -- just return the empty list.
%  FindResult = orddict:find(Id, Transitions),

generate_reachability_dict(OuterMonitorInstance) ->
  {_, Res} = reachable_from_inner(0, OuterMonitorInstance, orddict:new(), sets:new(), sets:new()),
  Res.

reachable_from_inner(NodeID, OuterMonitorInstance, Dict, CurrentReachableSet, Visited) ->
  % This will happen when we've visited the node before, but haven't necessarily
  % finished calculating the result for it (ie with recursion).
  % Simply return the empty set (it will be calculated once the call returns)
  io:format("Visiting node ~p...~n", [NodeID]),
  IsElement = sets:is_element(NodeID, Visited),
  if IsElement ->
       {CurrentReachableSet, Dict};
     true ->
       TransitionList =
        case orddict:find(NodeID, OuterMonitorInstance#monitor_instance.transitions) of
          {ok, Transitions} -> sets:to_list(Transitions);
          error -> []
        end,
       Node = orddict:fetch(NodeID, OuterMonitorInstance#monitor_instance.states),
       {_Ty, Id, _Info} = Node,
       % Firstly, check whether the reachable set is already in the dict.
       % If so, we can just return it.
       case orddict:find(Id, Dict) of
         {ok, ReachableSet} -> {ReachableSet, Dict};
         error ->
           % Right, if not...
           % Add the item to the visited set
           NewVisitedSet = sets:add_element(Id, Visited),
           % Get the roles involved with this role
           RolesInvolved = sets:from_list(roles_involved(Node)),
           io:format("Roles involved: ~p...~n", [sets:to_list(RolesInvolved)]),

           % Update the table for all outgoing transitions
           {RolesInOutgoing, NewDict} =
             lists:foldr(fun(NextID, {WorkingSet, WorkingDict}) ->
                            {ReachableOutgoingSet, NextDict} =
                              reachable_from_inner(NextID, OuterMonitorInstance,
                                            WorkingDict, sets:union(CurrentReachableSet, RolesInvolved),
                                            NewVisitedSet),
                              {sets:union(WorkingSet, ReachableOutgoingSet), NextDict} end,
                         {sets:new(), Dict}, TransitionList),
           % Now, add this node to the dictionary
           RolesInvolved1 = sets:union(RolesInvolved, RolesInOutgoing),
           NewDict1 = orddict:store(Id, RolesInvolved1, NewDict),
           {RolesInvolved1, NewDict1}
       end
  end.

is_role_reachable(RoleName, OuterMonitorInstance) ->
  CurrentState = OuterMonitorInstance#monitor_instance.current_state,
  ReachabilityDict = OuterMonitorInstance#monitor_instance.reachability_dict,
  ReachableRoles = orddict:fetch(CurrentState, ReachabilityDict),
  sets:is_element(RoleName, ReachableRoles).


