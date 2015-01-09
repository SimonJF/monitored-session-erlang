-module(monitor_gen).

% Generates a monitor from a Scribble AST.
%

% Sketch of a monitor node
% {id, node type, info}, where info is some kind of node-type-specific
% information.
%

make_node(Id, NodeType, Info) ->
  {node, Id, NodeType, Info}.

receive_node(Id, LocalReceive = {local_receive, MessageSig, Sender}) ->
  {message_signature, MessageName, PayloadTypes} = MessageSig,
  Info = {Sender, MessageName, PayloadTypes},
  make_node(Id, receive_node, Info).

send_node(Id, LocalSend = {local_send, MessageSig, Recipients}) ->
  {message_signature, MessageName, PayloadTypes} = MessageSig,
  Info = {Recipients, MessageName, PayloadTypes},
  make_node(Id, receive_node, PayloadTypes).

choice_node(Id, Choice) ->
  make_node(Id, choice_node, {}).

end_node(Id) -> make_node(Id, end_node, {}).


% Scopes!
% Each scope is a set of interactions, and there might be sub-scopes too.
% Importantly, the end of a scope has a pre-defined end index.
scope(Name, Instructions, EndIndex) ->
  {scope, Name, Instructions, EndIndex}.


% Calculates the size of a block of instructions (so we can do calculations
% on End indices)
block_size([]) -> 0;
block_size([X|XS]) -> instruction_size(X) + block_size(XS).


instruction_size({local_send, _, _}) -> 1;
instruction_size({local_receive, _, _}) -> 1;
instruction_size({choice, _, Choices}) ->
  1 + list:foldl(fun(Sum, ChoiceBlock) -> Sum + block_size(ChoiceBlock) end, 0, Choices);
instruction_size({recursion, _, Block}) -> block_size(Block);
instruction_size({par, ParallelBlocks}) -> 0; %FIXME: uhhhhh -- need to look up nested FSM stuff. God, writing this is going to take forever
instruction_size({local_interruptible, _, InterruptibleBlock, _}) -> % FIXME: No idea how to do this one either
  block_size(InterruptibleBlock);
instruction_size({local_interruptible_throw, _, InterruptibleBlock, _, _}) ->
  block_size(InterruptibleBlock);
% Do's just add a transition, without adding a node
instruction_size({do, _, _, _}) -> 0;
instruction_size({do_scope, _, _, _}) -> 0;
% Continue's just a transition.
instruction_size({continue, _}) -> 0.

node_type({local_send, _, _}) -> simple_transition;
node_type({local_receive, _, _}) -> simple_transition;
node_type({choice, _}) -> new_scope;
node_type({recursion, _}) -> new_scope;
node_type({parallel, _}) -> new_scope;
node_type({local_interruptible, _, _, _}) -> new_scope;
node_type({local_interruptible_throw, _, _, _, _}) -> new_scope;
node_type(_Other) -> other.

add_transition(From, To, TransitionTable) ->
      orddict:update(From, fun(OldTransitions) ->
                              sets:add_element(To, OldTransitions),
                              sets:from_list(To) end, TransitionTable).

% Scope -> RunningID -> States -> Transitions -> {ID, States, Transitions}
evaluate_scope(Scope = {scope, Instructions, EndIndex}, RunningID, States, Transitions) ->
  % Okay, so we're in the scope, have a running ID, state table and transition table.
  % We need to generate nodes for each of the instructions.
  % If the instruction is a send or receive, then it should have a transition to the
  % next instruction.
  % If the instruction is a choice, it should have transitions to the first instruction
  % of each sub-scope.
  % The final instruction should have a transition to the EndIndex.
  evaluate_scope_inner(Instructions, EndIndex, RunningID, States, Transitions).

evaluate_scope_inner([X], EndIndex, RunningID, States, Transitions) ->
  case node_type(X) of
    % Final node in scope, simple transition
    simple_transition ->
      % The last node needs to have a transition to the EndIndex
      {RID, NewStates, Ts} = generate_node(X, RunningID, States, Transitions),
      NewTransitions = add_transition(RunningID, EndIndex, Transitions),
      {RID, NewStates, NewTransitions};
    new_scope ->
      % Create a new scope, set the end index to our end index.
      case X of
        {choice, _RoleName, ChoiceBlocks} ->
          % For choices, we'll need to create new scopes for each choice block.
          % The end index of each should be EndIndex.
          ChoiceNodeID = RunningID,
          {RID1, States1, Transitions1} = generate_node(X, RunningID, States, Transitions),
          lists:foldl(fun ({RID, RStates, RTransitions}, Block) ->
                           Scope = scope("", Block, EndIndex),
                           % Add a transition from the choice node to this RID
                           % FIXME: What if scope is empty / has no commands which
                           % introduce new states? Dirty hack atm.
                           RTransitions1 = add_transition(ChoiceNodeID, RID, RTransitions),
                           evaluate_scope(Scope, RID, RStates, RTransitions1) end,
                      {RID1, States1, Transitions1}, ChoiceBlocks);
          % TODO: Recursion, Choice, Parallel, Interruptible
        Other -> io:format("Attempted to make node of unsupported type ~p~n", [Other])
      end
  end;
evaluate_scope_inner([X|XS], EndIndex, RunningID, States, Transitions) ->
  case node_type(X) of
    simple_transition ->
      % Node with simple +1 transition, and a node to transition to
      % Simple stuff, just need to generate a node and link to the ID + 1.
      % This is safe because we know the list has more than one entry :)
      {RID, NewStates, Ts} = generate_node(X, RunningID, States, Transitions),
      NextIndex = RID + 1,
      NewTransitions = add_transition(RID, NextIndex, Transitions),
      evaluate_scope_inner(XS, EndIndex, NextIndex, NewStates, NewTransitions);
     new_scope ->
      % Create a new scope, set the end index to our end index.
      case X of
        {choice, _RoleName, ChoiceBlocks} ->
          % For choices, we'll need to create new scopes for each choice block.
          % Calculate the end index, taking into account sub-blocks:
          EndIndex = instruction_size(X),
          ChoiceNodeID = RunningID,
          {RID1, States1, Transitions1} = generate_node(X, RunningID, States, Transitions),
          % Now, generate the remainder of the scope, with the calculated end index.
          lists:foldl(fun ({RID, RStates, RTransitions}, Block) ->
                           Scope = scope("", Block, EndIndex),
                           BlockSize = block_size(Block),
                           RTransitions1 = if BlockSize > 0 ->
                                                add_transition(ChoiceNodeID, RID, RTransitions);
                                              BlockSize == 0 ->
                                                RTransitions
                                           end,
                           evaluate_scope(Scope, RID, RStates, RTransitions1) end,
                      {RID1, States1, Transitions1}, ChoiceBlocks);
          % TODO: Recursion, Choice, Parallel, Interruptible
        Other -> io:format("Attempted to make node of unsupported type ~p~n", [Other])
      end
  end.


% Generates a monitor node, given the AST of a projected local type.
% generate_node/4 : LocalAST -> Id -> State table -> Transition table ->
%   (New ID, New states, New transitions)
generate_node(LocalRecv = {local_receive, _, _}, RunningID, States, Transitions) ->
  RecvNode = receive_node(RunningID, LocalRecv),
  NewStates = orddict:append(RunningID, RecvNode, States),
  {RunningID + 1, NewStates, Transitions};
generate_node(LocalSend = {local_send, _, _}, RunningID, States, Transitions) ->
  SendNode = send_node(RunningID, LocalSend),
  NewStates = orddict:append(RunningID, SendNode, States),
  {RunningID + 1, NewStates, Transitions}.
generate_node(Choice = {choice, _, _}, RunningID, States, Transitions) ->
  ChoiceNode = choice_node(RunningID, Choice),
  NewStates = orddict:append(RunningID, ChoiceNode, States),
  {RunningID + 1, NewStates, Transitions}.
%generate_monitor(LocalProtocol = {local_protocol, _, _, _, _, Interactions}, RunningID, States, Transitions) ->
%  {}.
  % Generates a monitor for the local protocol block. This basically involves
  % doing a fold over the interactions.
  %lists:foldl(fun (Interaction, {RunningID, Ss, Ts}) ->
  %                generate_monitor(Interaction, RunningID, Ss, Ts) end,
  %            RunningID, States, Transitions);
  %
% We can't do monitors for other AST constructs (ie global types)
%generate_monitor(LocalAST, _, _, _) ->
%  {error, monitor_undefined, LocalAST}.


% Checks whether we can send or receive at this point
can_receive(Message, MonitorNode = {Id, receive_node, Info}) ->
  {Sender, MessageName, PayloadTypes} = Info,
  CorrectSender = message:message_sender(Message) == Sender,
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = message:message_payload_types(Message) == PayloadTypes,
  case {CorrectSender, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> {true};
    {false, _, _} -> {false, bad_sender};
    {_, false, _} -> {false, bad_message_name};
    {_, _, false} -> {false, bad_payload_types}
  end;
can_receive(_Message, MonitorNode) ->
  {false, bad_node_type, MonitorNode}.


can_send(Message, MonitorNode = {Id, send_node, Info}) ->
  {Recipients, MessageName, PayloadTypes} = Info,
  CorrectRecipients = lists:sort(Recipients) == lists:sort(message:message_recipients(Message)),
  CorrectMessageName = message:message_name(Message) == MessageName,
  CorrectPayloadTypes = message:message_payload_types(Message) == PayloadTypes,
  case {CorrectRecipients, CorrectMessageName, CorrectPayloadTypes} of
    {true, true, true} -> {true};
    {false, _, _} -> {false, bad_recipients};
    {_, false, _} -> {false, bad_message_name};
    {_, _, false} -> {false, bad_payload_types}
  end;
can_send(_Message, MonitorNode) ->
  {false, bad_node_type, MonitorNode}.

