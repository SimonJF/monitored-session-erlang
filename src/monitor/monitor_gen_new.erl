-module(monitor_gen_new).
-compile(export_all).
-include("monitor_records.hrl").

%%%%%%%%
%%% Top-level generation functions
%%%%%%%%

print_fsms(NestedFSMs) ->
  NestedFSMList = orddict:to_list(NestedFSMs),
  lists:foreach(fun({ID, FSM}) ->
                    io:format("FSM ID: ~p~n", [ID]),
                    States = FSM#monitor_gen_state.states,
                    Transitions = FSM#monitor_gen_state.transitions,
                    print_fsm(States, Transitions) end, NestedFSMList).

graphviz_out(Transitions) ->
  io:format("digraph G {~n", []),
  orddict:fold((fun (S, TSet, _Acc) ->
                   % io:format("~p: ~w~n", [S, sets:to_list(TSet)]),
                   lists:foreach(fun ({_, ToID, _, _, _}) ->
                                     io:format("~w -> ~w~n", [S, ToID]) end, sets:to_list(TSet)),
               {} end), {}, Transitions),
  io:format("}~n", []).

% hax to the max
print_fsm(States, Transitions) ->
  io:format("States: ~n", []),
  orddict:fold(fun (K, V, _Acc) ->
                   io:format("~p: ~p~n", [K, V]),
                   {} end, {}, States),

  io:format("Transitions: ~n", []),
  orddict:fold(fun (S, TSet, _Acc) ->
                   io:format("~p: ~p~n", [S, sets:to_list(TSet)]),
                   {} end, {}, Transitions),
  io:format("Graphviz output:~n"),
  graphviz_out(Transitions).


test_fsm(Filename) ->
  io:format("In test_fsm~n"),
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
                                  {ok, ProtocolName, {ok, OuterState}} ->
                                    io:format("Monitor for local protocol ~s:~n", [ProtocolName]),
                                    print_fsms(OuterState);
                                    % print_fsm(States, Transitions),
                                    %print_reachability(States, Transitions);
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
      io:format("WARN: Multiple matching monitors for ~s:~s~n", [ProtocolName, RoleName]),
      generate_monitor(X)
  end.

fresh_outer_state() ->
  #outer_monitor_gen_state{nested_fsms=orddict:new(),
                           running_nested_fsm_id=0}.

fresh_inner_state() ->
  #monitor_gen_state{states=orddict:new(),
                     transitions=orddict:new(),
                     running_id=0}.


increment_running_fsm_id(MonitorState) ->
  OldID = MonitorState#outer_monitor_gen_state.running_nested_fsm_id,
  MonitorState#outer_monitor_gen_state{running_nested_fsm_id=OldID + 1}.

update_nested_fsm(FSMID, FSMState, MonitorState) ->
  OldNestedFSMs = MonitorState#outer_monitor_gen_state.nested_fsms,
  NewNestedFSMs = orddict:store(FSMID, FSMState, OldNestedFSMs),
  MonitorState#outer_monitor_gen_state{nested_fsms=NewNestedFSMs}.

evaluate_nested_fsm(Block, FSMID, MonitorState) ->
  % Create an outer monitor gen state.
  FSMState = fresh_inner_state(),
  % Create the start state
  FSMState1 = add_state(standard_state(), FSMState),
  MonitorState1 = update_nested_fsm(FSMID, FSMState1, MonitorState),
  MonitorState2 = increment_running_fsm_id(MonitorState1),
% evaluate_block([X|XS], PrevIndex, EndIndex, FSMState, MonitorState) ->

  ScopeEndIndex = block_size(Block) + 1,
  EvalRes = evaluate_block(Block, 0, ScopeEndIndex, FSMState1, MonitorState2),
  case EvalRes of
    {FSMState2, MonitorState3} ->
      FSMState3 = add_state(end_state(), FSMState2),
      {ok, update_nested_fsm(FSMID, FSMState3, MonitorState3)};
    Other -> Other % Various monitor errors
  end.

generate_monitor({local_protocol, _, _, _, _, Block}) ->
  MonitorState = fresh_outer_state(),
  EvalRes = evaluate_nested_fsm(Block, 0, MonitorState),
  case EvalRes of
    {ok, MonitorGenState1} ->
      {ok, MonitorGenState1#outer_monitor_gen_state.nested_fsms};
    Other -> Other
  end.

%%%%%%%%
%%% Transition Construction Functions
%%%%%%%%

%%%% Communication Actions
send_transition(ToID, Recipients, MessageName, PayloadTypes) ->
  {send_transition, ToID, Recipients, MessageName, PayloadTypes}.

receive_transition(ToID, Sender, MessageName, PayloadTypes) ->
  {receive_transition, ToID, Sender, MessageName, PayloadTypes}.

send_call_request_transition(ToID, Recipient, MessageName, PayloadTypes) ->
  {send_call_request_transition, ToID, Recipient, MessageName, PayloadTypes}.

receive_call_request_transition(ToID, Sender, MessageName, PayloadTypes) ->
  {receive_call_request_transition, ToID, Sender, MessageName, PayloadTypes}.

send_call_response_transition(ToID, Recipient, MessageName, PayloadTypes) ->
  {send_call_response_transition, ToID, Recipient, MessageName, PayloadTypes}.

receive_call_response_transition(ToID, Sender, MessageName, PayloadTypes) ->
  {receive_call_response_transition, ToID, Sender, MessageName, PayloadTypes}.

join_transition(ToID) ->
  {join_transition, ToID}.

%%% State creation functions: Standard state, and a state "containing" nested FSMs
standard_state() -> standard_state.
par_state(NestedFSMIDs) -> {par_state, NestedFSMIDs}.
end_state() -> end_state.

%%%%%%%%
%%% Internal Monitor Generation Functions
%%%%%%%%

block_size([]) -> 0;
block_size([X]) ->
  instruction_size(X);
block_size([X|XS]) ->
  instruction_size(X) + 1 + block_size(XS).

instruction_size({choice, _, Choices}) ->
  lists:foldl(fun(ChoiceBlock, Sum) ->
              Sum + (block_size(ChoiceBlock)) end, 0, Choices);
instruction_size(_) -> 0.

% instruction_size({local_send, _, _}) -> 1;
% instruction_size({local_receive, _, _}) -> 1;
% instruction_size({local_call_request_send, _, _}) -> 1;
% instruction_size({local_call_request_recv, _, _}) -> 1;
% instruction_size({local_call_response_send, _, _}) -> 1;
% instruction_size({local_call_response_recv, _, _}) -> 1;
% instruction_size({choice, _, Choices}) ->
%   % Final transition in the choice block goes to the end index
%   BasicSize = lists:foldl(fun(ChoiceBlock, Sum) -> Sum + (block_size(ChoiceBlock)) end, 0, Choices),
%   BasicSize;
% instruction_size({rec, _, Block}) -> 1 + block_size(Block);
% instruction_size({par, _ParallelBlocks}) -> 1; % Nested FSMs have their own internal numbering system
% instruction_size({local_interruptible, _, InterruptibleBlock, _}) -> % Not supported.
%   block_size(InterruptibleBlock);
% instruction_size({local_interruptible_throw, _, InterruptibleBlock, _, _}) ->
%   block_size(InterruptibleBlock);
% % Do's just add a transition, without adding a node
% instruction_size({do, _, _, _}) -> 0;
% instruction_size({do_scope, _, _, _}) -> 0;
% % Continue's just a transition.
% instruction_size({continue, _}) -> 0.


% add_state: Takes FSMState, returns (new state ID, new FSMState)
add_state(State, FSMState) ->
  RunningID = FSMState#monitor_gen_state.running_id,
  States = FSMState#monitor_gen_state.states,
  NewStates = orddict:store(RunningID, State, States),
  NewRunningID = RunningID + 1,
  FSMState#monitor_gen_state{states=NewStates, running_id=NewRunningID}.

% Takes FromID, transition to add, and current FSM state.
% Returns updated FSM state.
add_transition(FromID, Transition, FSMState) ->
  Transitions = FSMState#monitor_gen_state.transitions,
  NewTransitions =
    orddict:update(FromID, fun(OldTransitionSet) ->
                               sets:add_element(Transition, OldTransitionSet) end,
                               sets:from_list([Transition]), Transitions),
  FSMState#monitor_gen_state{transitions=NewTransitions}.

% Returns true if the node is a comm action (send / recv)
is_comm_action({local_send, _, _}) -> true;
is_comm_action({local_receive, _, _}) -> true;
is_comm_action({local_call_request_send, _, _}) -> true;
is_comm_action({local_call_request_recv, _, _}) -> true;
is_comm_action({local_call_response_send, _, _}) -> true;
is_comm_action({local_call_response_recv, _, _}) -> true;
is_comm_action(_) -> false.


unpack_sig({message_signature, Name, Types}) -> {Name, Types}.

% Returns a new FSMState
add_comm_transition({local_send, Sig, Participants}, FromID, ToID, FSMState) ->
  {Name, Types} = unpack_sig(Sig),
  add_transition(FromID, send_transition(ToID, Participants, Name, Types), FSMState);
add_comm_transition({local_receive, Sig, Participants}, FromID, ToID, FSMState) ->
  {Name, Types} = unpack_sig(Sig),
  add_transition(FromID, receive_transition(ToID, Participants, Name, Types), FSMState);
add_comm_transition({local_call_request_send, Sig, Participants}, FromID, ToID, FSMState) ->
  {Name, Types} = unpack_sig(Sig),
  add_transition(FromID, send_call_request_transition(ToID, Participants, Name, Types), FSMState);
add_comm_transition({local_call_request_recv, Sig, Participants}, FromID, ToID, FSMState) ->
  {Name, Types} = unpack_sig(Sig),
  add_transition(FromID, receive_call_request_transition(ToID, Participants, Name, Types), FSMState);
add_comm_transition({local_call_response_send, Sig, Participants}, FromID, ToID, FSMState) ->
  {Name, Types} = unpack_sig(Sig),
  add_transition(FromID, send_call_response_transition(ToID, Participants, Name, Types), FSMState);
add_comm_transition({local_call_response_recv, Sig, Participants}, FromID, ToID, FSMState) ->
  {Name, Types} = unpack_sig(Sig),
  add_transition(FromID, receive_call_response_transition(ToID, Participants, Name, Types), FSMState);
add_comm_transition(_, _, _, _) -> error(add_bad_transition).

get_running_id(FSMState) -> FSMState#monitor_gen_state.running_id.

is_list_empty([]) -> true;
is_list_empty(_) -> false.

% Takes AST node detailing comm action, bool detailing whether comm is last in the block,
% end index of the block, and FSM state.
% Returns next ID, FSM with either a new state and a transition to it, or a transition
%  to the end state added.
evaluate_comm_transition(CommAST, PrevID, FSMState) ->
  RunningID = get_running_id(FSMState),
  FSMState1 = add_state(standard_state(), FSMState),
  add_comm_transition(CommAST, PrevID, RunningID, FSMState1).



% Takes the ID of the previously-created node, end index, FSM state, monitor state.
% On each step?
% If final instruction in block and comm, make transition from PrevID to EndIndex.
% If final instruction in block and not comm, eval state as normal?
% If not final instruction in block, create new state, make transition from prev
% to new, predicated on the comm action, then recursively evaluate.
% Return FSMState and MonitorState.
%
evaluate_block([], _PrevIndex, _EndIndex, FSMState, MonitorState) ->
  {FSMState, MonitorState};
evaluate_block([X], PrevIndex, EndIndex, FSMState, MonitorState) ->
  IsCommAction = is_comm_action(X),
  if IsCommAction ->
       % Add transition to the end node of the scope
       FSMState1 = add_comm_transition(X, PrevIndex, EndIndex, FSMState),
       {FSMState1, MonitorState};
     not IsCommAction ->
       evaluate_scope(X, PrevIndex, EndIndex, FSMState, MonitorState)
  end;
evaluate_block([X|XS], PrevIndex, EndIndex, FSMState, MonitorState) ->
  IsCommAction = is_comm_action(X),
  RunningID = get_running_id(FSMState),
  if IsCommAction ->
       % Add a new state, and make a transition to it.
       FSMState1 = evaluate_comm_transition(X, PrevIndex, FSMState),
       evaluate_block(XS, RunningID, EndIndex, FSMState1, MonitorState);
     not IsCommAction ->
       ScopeEndIndex = RunningID + instruction_size(X),
       {FSMState1, MonitorState1} =
         evaluate_scope(X, PrevIndex, ScopeEndIndex, FSMState, MonitorState),
       FSMState2 = add_state(standard_state(), FSMState1),
       % PrevIndex here: end index of the scope.
       evaluate_block(XS, ScopeEndIndex, EndIndex,
                      FSMState2, MonitorState1)
  end.


% Evaluates a list of blocks.
% Returns an FSMState and MonitorState, and a list of top IDs.
evaluate_blocks([], _PrevIndex, _EndIndex, FSMState, MonitorState) ->
  {FSMState, MonitorState};
evaluate_blocks([Block|Blocks], PrevIndex, EndIndex, FSMState, MonitorState) ->
  {FSMState1, MonitorState1} = evaluate_block(Block, PrevIndex, EndIndex, FSMState, MonitorState),
  evaluate_blocks(Blocks, PrevIndex, EndIndex, FSMState1, MonitorState1).

% Takes top-level scope AST, FSM state and monitor state.
% Returns ID of top node, new FSM state, new monitor state.
evaluate_scope({local_protocol, _, _, _, _, Block}, _PrevIndex, _EndIndex, FSMState,
               MonitorState) ->
  ScopeEndIndex = instruction_size(Block),
  {FSMState1, MonitorState1} = evaluate_block(Block, 0, ScopeEndIndex, FSMState, MonitorState),
  FSMState2 = add_state(standard_state(), FSMState1),
  {FSMState2, MonitorState1};
evaluate_scope({choice, _, ChoiceBlocks}, PrevIndex, EndIndex,
               FSMState, MonitorState) ->
  % We have a choice block.
  % We need to evaluate each block in turn.
  % Each block needs to converge on the end index.
  evaluate_blocks(ChoiceBlocks, PrevIndex, EndIndex, FSMState, MonitorState).















  % Running ID: Last state + 1
  %RunningID = get_running_id(FSMState),
  %io:format("RunningID: ~p~nChoice instruction size: ~p~n", [RunningID, instruction_size(Choice)]),
  %ScopeEndIndex = RunningID + instruction_size(Choice),
  %{FSMState1, MonitorState1} = evaluate_blocks(ChoiceBlocks, PrevIndex,
       %                                        ScopeEndIndex, FSMState, MonitorState),
  % Now, we need a state at ScopeEndIndex (which *should* be RunningID)
  % This is one place bugs could creep in...
  %io:format("RunningID = ~p, ScopeEndIndex = ~p~n", [get_running_id(FSMState1), ScopeEndIndex]),
  %FSMState2 = add_state(standard_state(), FSMState1),
  %{FSMState2, MonitorState1}.

