-module(monitor_gen).
-compile(export_all).
-define(LOCAL_PROTOCOL_FILE, "test.scr").

% monitor_gen: Functions for generating and manipulating FSM-based monitors.

% Generates a monitor from a Scribble AST.
%

% Sketch of a monitor node
% {id, node type, info}, where info is some kind of node-type-specific
% information.
%

make_node(NodeType, Id, Info) ->
  {NodeType, Id, Info}.

receive_node(Id, {local_receive, MessageSig, Sender}) ->
  {message_signature, MessageName, PayloadTypes} = MessageSig,
  Info = {Sender, MessageName, PayloadTypes},
  make_node(receive_node, Id, Info).

send_node(Id, {local_send, MessageSig, Recipients}) ->
  {message_signature, MessageName, PayloadTypes} = MessageSig,
  Info = {Recipients, MessageName, PayloadTypes},
  make_node(send_node, Id, Info).

choice_node(Id, _Choice) ->
  make_node(choice_node, Id, {}).

end_node(Id) -> make_node(end_node, Id, {}).


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
  1 + lists:foldl(fun(ChoiceBlock, Sum) -> Sum + block_size(ChoiceBlock) end, 0, Choices);
instruction_size({recursion, _, Block}) -> block_size(Block);
instruction_size({par, _ParallelBlocks}) -> 0; %FIXME: uhhhhh -- need to look up nested FSM stuff. God, writing this is going to take forever
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
node_type({choice, _, _}) -> new_scope;
node_type({recursion, _}) -> new_scope;
node_type({parallel, _}) -> new_scope;
node_type({local_interruptible, _, _, _}) -> new_scope;
node_type({local_interruptible_throw, _, _, _, _}) -> new_scope;
node_type(_Other) -> io:format("Other: ~p~n", [_Other]),
                     other.


% Internal quick 'n' easy test harness
% Will probably delete this at some point
test_fsm(Filename) ->
  ParseResult = scribble_lexer:parse(Filename),
  case ParseResult of
    {error, file_open_error, Error} ->
      io:format("Error opening file ~s: ~s", [Filename, Error]);
    {error, lex_error, Error} ->
      io:format("Error lexing file ~s : ~s", [Filename, Error]);
    {error, parse_error, Error} ->
      io:format("Error parsing file ~s : ~s", [Filename, Error]);
    {ok, AST } -> LocalMonitors = generate_module_monitors(AST),
                  io:format("AST: ~p~n", [AST]),
                  lists:foreach(fun (Mon) ->
                                case Mon of
                                  {ok, ProtocolName, {ok, {_RID, States, Transitions}}} ->
                                    io:format("Monitor for local protocol ~s:~n", [ProtocolName]),
                                    print_fsm(States, Transitions),
                                    graphviz_out(States, Transitions);
                                  {ok, ProtocolName, Error} ->
                                    io:format("Error generating monitor for local protocol ~s:~n~p~n",
                                              [ProtocolName, Error]);
                                  {error, invalid_protocol, AST} ->
                                    io:format("Error: ~p is not a local protocol ~n", [AST]);
                                  {error, monitor_gen, AST} ->
                                    io:format("Error: Could not generate monitor for ~p~n", [AST])
                                end
                                end, LocalMonitors)

  end.

generate_module_monitors({module, _Name, _Imports, _Payloads, Protocols}) ->
  lists:map(fun (Protocol) ->
                case Protocol of
                  {local_protocol, ProtocolName, _, _, _, _} ->
                    {ok, ProtocolName, generate_monitor(Protocol)};
                  Other ->
                    {error, invalid_protocol, Other}
                end
            end, Protocols);
generate_module_monitors(_AST) ->
  {error, not_a_module}.


generate_monitor_for({module, _Name, _Imports, _Payloads, Protocols},
                     ProtocolName, RoleName) ->
  FilteredList =
    lists:filter(fun (Protocol) ->
                     % First, only work with local protocols
                     case Protocol of
                       {local_protocol, PName, RName, _, _, _} ->
                         (PName == ProtocolName) and (RName == RoleName);
                       _Other -> false
                     end end, Protocols),
  case FilteredList of
    [] -> {error, no_matching_protocol};
    [X] -> generate_monitor(X);
    [X|_XS] ->
      % Really, there shouldn't be more than one. We could error out,
      % but here I'll just warn and opt to take the first.
      % I am a gracious god.
      io:format("WARN: Multiple matching monitors for ~s:~s~n", [ProtocolName, RoleName]),
      generate_monitor(X)
  end.


% Generates a monitor from a local protocol. Only works with a local protocol definition!
generate_monitor({local_protocol, ProtocolName, ProjRoleName, _Params, _Roles, Block}) ->
  Size = block_size(Block),
  RootScope = scope(ProtocolName ++ "@" ++ ProjRoleName, Block, Size),
  EvalRes = evaluate_scope(RootScope, 0, orddict:new(), orddict:new()),
  case EvalRes of
    {EndID, States1, Transitions1} ->
      {ok, add_node(EndID, end_node(EndID), States1, Transitions1)};
    Other -> Other % Various monitor errors
  end;
generate_monitor(_Other) -> undefined.

calculate_next_index([], RunningID, _EndIndex) ->
  RunningID;
calculate_next_index([_X], _RunningID, EndIndex) ->
  EndIndex;
calculate_next_index([_X|_XS], RunningID, _EndIndex) ->
  RunningID + 1.

add_transition(From, To, TransitionTable) ->
      orddict:update(From, fun(OldTransitions) ->
                              sets:add_element(To, OldTransitions) end,
                              sets:from_list([To]), TransitionTable).

% Scope -> RunningID -> States -> Transitions -> {ID, States, Transitions}
evaluate_scope({scope, _Name, Instructions, EndIndex}, RunningID, States, Transitions) ->
  % Okay, so we're in the scope, have a running ID, state table and transition table.
  % We need to generate nodes for each of the instructions.
  % If the instruction is a send or receive, then it should have a transition to the
  % next instruction.
  % If the instruction is a choice, it should have transitions to the first instruction
  % of each sub-scope.
  % The final instruction should have a transition to the EndIndex.
  evaluate_scope_inner(Instructions, EndIndex, RunningID, States, Transitions).


evaluate_scope_inner([], _EI, RunningID, States, Transitions) ->
  {RunningID, States, Transitions};
evaluate_scope_inner(ScopeBlock = [X|XS], EndIndex, RunningID, States, Transitions) ->
  case node_type(X) of
    simple_transition ->
      % Node with simple +1 transition, and a node to transition to
      % Simple stuff, just need to generate a node and link to the ID + 1.
      % This is safe because we know the list has more than one entry :)
      {RID, NewStates, Ts} = generate_node(X, RunningID, States, Transitions),
      NextIndex = calculate_next_index(ScopeBlock, RunningID, EndIndex),
      NewTransitions = add_transition(RunningID, NextIndex, Ts),
      evaluate_scope_inner(XS, EndIndex, RID, NewStates, NewTransitions);
     new_scope ->
      % Create a new scope, set the end index to our end index.
      case X of
        {choice, _RoleName, ChoiceBlocks} ->
          % For choices, we'll need to create new scopes for each choice block.
          % Calculate the end index, taking into account sub-blocks:
          ScopeEndIndex = RunningID + instruction_size(X),
          ChoiceNodeID = RunningID,
          {RID1, States1, Transitions1} = generate_node(X, RunningID, States, Transitions),
          % Now, generate the remainder of the scope, with the calculated end index.
          {RID2, States2, Transitions2} = lists:foldl(fun (Block, {RID, RStates, RTransitions}) ->
                           Scope = scope("", Block, ScopeEndIndex),
                           BlockSize = block_size(Block),
                           RTransitions1 = if BlockSize > 0 ->
                                                add_transition(ChoiceNodeID, RID, RTransitions);
                                              BlockSize == 0 ->
                                                RTransitions
                                           end,
                           evaluate_scope(Scope, RID, RStates, RTransitions1) end,
                      {RID1, States1, Transitions1}, ChoiceBlocks),
          evaluate_scope_inner(XS, EndIndex, RID2, States2, Transitions2);
          % TODO: Recursion, Choice, Parallel, Interruptible
        Other -> {error, monitor_gen, unsupported_node, Other}
      end;
     Other -> {error, monitor_gen, unsupported_node, Other}
  end.


% Generates a monitor node, given the AST of a projected local type.
% generate_node/4 : LocalAST -> Id -> State table -> Transition table ->
%   (New ID, New states, New transitions)
%
add_node(ID, Node, States, Transitions) ->
  NewStates = orddict:store(ID, Node, States),
  {ID + 1, NewStates, Transitions}.

generate_node(LocalRecv = {local_receive, _, _}, RunningID, States, Transitions) ->
  RecvNode = receive_node(RunningID, LocalRecv),
  add_node(RunningID, RecvNode, States, Transitions);
generate_node(LocalSend = {local_send, _, _}, RunningID, States, Transitions) ->
  SendNode = send_node(RunningID, LocalSend),
  add_node(RunningID, SendNode, States, Transitions);
generate_node(Choice = {choice, _, _}, RunningID, States, Transitions) ->
  ChoiceNode = choice_node(RunningID, Choice),
  add_node(RunningID, ChoiceNode, States, Transitions).

% Debug all the things
% hax to the max
print_fsm(States, Transitions) ->
  io:format("States: ~n", []),
  orddict:fold(fun (K, V, _Acc) ->
                   io:format("~p: ~p~n", [K, V]),
                   {} end, {}, States),

  io:format("Transitions: ~n", []),
  orddict:fold(fun (S, TSet, _Acc) ->
                   io:format("~p: ~w~n", [S, sets:to_list(TSet)]),
                   {} end, {}, Transitions).

graphviz_out(_States, Transitions) ->
  io:format("digraph G {~n", []),
  orddict:fold((fun (S, TSet, _Acc) ->
                   % io:format("~p: ~w~n", [S, sets:to_list(TSet)]),
                   lists:foreach(fun (OutTrans) ->
                                     io:format("~w -> ~w~n", [S, OutTrans]) end, sets:to_list(TSet)),
               {} end), {}, Transitions),
  io:format("}~n", []).

