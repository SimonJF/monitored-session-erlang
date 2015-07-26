-module(monitor_gen).
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
                   lists:foreach(fun (T) ->
                                     ToID = element(2, T),
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
                                  {ok, ProtocolName, {ok, Monitor}} ->
                                    io:format("Monitor for local protocol ~s:~n", [ProtocolName]),
                                    print_fsms(Monitor),
                                    % print_fsm(States, Transitions),
                                    print_reachability(Monitor);
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

print_reachability(Monitor) ->
  io:format("Printing reachability...~n"),
  MonitorInstance = monitor:create_monitor_instance("", "", Monitor),
  % TODO: Will only have root -- others aren't statically initialised
  lists:foreach(fun({FSMID, FSM}) ->
    ReachabilityDictList = orddict:to_list(FSM#monitor_instance.reachability_dict),
    lists:foreach(fun ({StateNum, {RoleSet, FSMSet}}) ->
                      RoleList = sets:to_list(RoleSet),
                      FSMList = sets:to_list(FSMSet),
                      io:format("~p: Roles: ~p, Nested FSMS: ~p~n", [StateNum,
                                                                     RoleList,
                                                                     FSMList]) end,
                  ReachabilityDictList) end,
                orddict:to_list(MonitorInstance#outer_monitor_instance.monitor_instances)).

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
  EvalRes = evaluate_block(Block, 0, ScopeEndIndex, orddict:new(), FSMState1, MonitorState2),
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
  {send, ToID, Recipients, MessageName, PayloadTypes}.

receive_transition(ToID, Sender, MessageName, PayloadTypes) ->
  {recv, ToID, Sender, MessageName, PayloadTypes}.

send_call_request_transition(ToID, Recipient, MessageName, PayloadTypes) ->
  {send_call_request, ToID, Recipient, MessageName, PayloadTypes}.

receive_call_request_transition(ToID, Sender, MessageName, PayloadTypes) ->
  {recv_call_request, ToID, Sender, MessageName, PayloadTypes}.

send_call_response_transition(ToID, Recipient, MessageName, PayloadTypes) ->
  {send_call_response, ToID, Recipient, MessageName, PayloadTypes}.

receive_call_response_transition(ToID, Sender, MessageName, PayloadTypes) ->
  {recv_call_response, ToID, Sender, MessageName, PayloadTypes}.

par_transition(ToID, NestedFSMIDs) ->
  {par_transition, ToID, NestedFSMIDs}.

start_subsession_transition(ToID, SubsessionName, RoleInstantiationList) ->
  {ExternalRoles, InternalRoles} = get_instantiate_roles(RoleInstantiationList),
  {start_subsession, ToID, SubsessionName, InternalRoles, ExternalRoles}.

subsession_success_transition(ToID) ->
  {subsession_success, ToID}.

subsession_failure_transition(ToID, FailureName) ->
  {subsession_failure, ToID, FailureName}.

%%% State creation functions: Standard state, and a state "containing" nested FSMs
standard_state() -> standard_state.
%par_state(NestedFSMIDs) -> {par_state, NestedFSMIDs}.
end_state() -> end_state.

%%%%%%%%
%%% Internal Monitor Generation Functions
%%%%%%%%
get_instantiate_roles(XS) -> get_instantiate_roles_inner(XS, [], []).

get_instantiate_roles_inner([], External, Internal) -> {External, Internal};
get_instantiate_roles_inner([{role_instantiation, Name}|XS], External, Internal) ->
  get_instantiate_roles_inner(XS, [Name|External], Internal);
get_instantiate_roles_inner([{new_role_instantiation, Name}|XS], External, Internal) ->
  get_instantiate_roles_inner(XS, External, [Name|Internal]).

block_size([]) -> 0;
block_size([X]) ->
  instruction_size(X);
block_size([_|[{continue, _}|_]]) -> 0;
block_size([X|XS]) ->
  instruction_size(X) + 1 + block_size(XS).

instruction_size({rec, _, Block}) ->
  block_size(Block);
instruction_size({local_initiates, _, SN, _, SuccessBlock, HandleBlocks}) ->
  HandleBlocksSize = lists:foldl(fun(HB = {handle_block, _, Block}, Sum) ->
                                     Sum + initiates_size(Block) end, 0, HandleBlocks),
  SuccessBlockSize = initiates_size(SuccessBlock),
  HandleBlocksSize + SuccessBlockSize + 1;
instruction_size({choice, _, Choices}) ->
  lists:foldl(fun(ChoiceBlock, Sum) ->
              Sum + (block_size(ChoiceBlock)) end, 0, Choices);
instruction_size(Other) -> 0.

initiates_size([]) -> 0;
initiates_size([{continue, _}|_]) -> 0;
initiates_size([X]) -> 1 + instruction_size(X);
initiates_size([X|XS]) -> 1 + block_size([X|XS]).

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
is_comm_action({par, _}) -> true;
is_comm_action(_) -> false.


unpack_sig({message_signature, Name, Types}) -> {Name, Types}.

% Returns a new FSMState
add_comm_transition({local_send, Sig, Participants}, FromID, ToID, FSMState, MonitorState) ->
  {Name, Types} = unpack_sig(Sig),
  FSMState1 = add_transition(FromID, send_transition(ToID, Participants, Name, Types), FSMState),
  {FSMState1, MonitorState};
add_comm_transition({local_receive, Sig, Participants}, FromID, ToID, FSMState, MonitorState) ->
  {Name, Types} = unpack_sig(Sig),
  FSMState1 = add_transition(FromID, receive_transition(ToID, Participants, Name, Types), FSMState),
  {FSMState1, MonitorState};
add_comm_transition({local_call_request_send, Sig, Participants}, FromID, ToID, FSMState, MonitorState) ->
  {Name, Types} = unpack_sig(Sig),
  FSMState1 = add_transition(FromID, send_call_request_transition(ToID, Participants, Name, Types), FSMState),
  {FSMState1, MonitorState};
add_comm_transition({local_call_request_recv, Sig, Participants}, FromID, ToID, FSMState, MonitorState) ->
  {Name, Types} = unpack_sig(Sig),
  FSMState1 = add_transition(FromID, receive_call_request_transition(ToID, Participants, Name, Types), FSMState),
  {FSMState1, MonitorState};
add_comm_transition({local_call_response_send, Sig, Participants}, FromID, ToID, FSMState, MonitorState) ->
  {Name, Types} = unpack_sig(Sig),
  FSMState1 = add_transition(FromID, send_call_response_transition(ToID, Participants, Name, Types), FSMState),
  {FSMState1, MonitorState};
add_comm_transition({local_call_response_recv, Sig, Participants}, FromID, ToID, FSMState, MonitorState) ->
  {Name, Types} = unpack_sig(Sig),
  FSMState1 = add_transition(FromID, receive_call_response_transition(ToID, Participants, Name, Types), FSMState),
  {FSMState1, MonitorState};
add_comm_transition({par, ParallelBlocks}, FromID, ToID, FSMState, MonitorState) ->
  {MonitorState1, NestedFSMIDs} =
    evaluate_parallel_blocks(ParallelBlocks, MonitorState),
  FSMState1 = add_transition(FromID, par_transition(ToID, NestedFSMIDs), FSMState),
  {FSMState1, MonitorState1};
add_comm_transition({local_initiates, _, SubsessionName, RIL, _, _},
                              FromID, ToID, FSMState, MonitorState) ->
  NewTransition = start_subsession_transition(ToID, SubsessionName, RIL),
  FSMState1 = add_transition(FromID, NewTransition, FSMState),
  {FSMState1, MonitorState};
add_comm_transition(_, _, _, _, _) -> error(add_bad_transition).

get_running_id(FSMState) -> FSMState#monitor_gen_state.running_id.

is_list_empty([]) -> true;
is_list_empty(_) -> false.

% Takes AST node detailing comm action, bool detailing whether comm is last in the block,
% end index of the block, and FSM state.
% Returns next ID, FSM with either a new state and a transition to it, or a transition
%  to the end state added.

evaluate_transition(X, [{continue, RecName}|_], PrevID, _EndIndex,
                    RecMap, FSMState, MonitorState) ->
  evaluate_rec_transition(X, PrevID, RecName, RecMap, FSMState, MonitorState);
%evaluate_transition(X, [{par, ParallelBlocks}|XS], PrevID, EndIndex, RecMap, FSMState,
%                    MonitorState) ->
%  RunningID = get_running_id(FSMState),
%
%  {MonitorState1, NestedFSMIDs} = evaluate_parallel_blocks(ParallelBlocks,
%                                                           MonitorState, RecMap),
%  FSMState1 = evaluate_comm_transition(X, PrevID, par_state(NestedFSMIDs), FSMState),
%  evaluate_block(XS, RunningID, EndIndex, RecMap, FSMState1, MonitorState1);
evaluate_transition(X, XS, PrevID, EndIndex, RecMap, FSMState, MonitorState) ->
  RunningID = get_running_id(FSMState),
  {FSMState1, MonitorState1} =
    evaluate_comm_transition(X, PrevID, standard_state(), FSMState, MonitorState),
  evaluate_block(XS, RunningID, EndIndex, RecMap, FSMState1, MonitorState1).


evaluate_comm_transition(CommAST, PrevID, StateToAdd, FSMState, MonitorState) ->
  RunningID = get_running_id(FSMState),
  FSMState1 = add_state(StateToAdd, FSMState),
  add_comm_transition(CommAST, PrevID, RunningID, FSMState1, MonitorState).

evaluate_rec_transition(CommAST, PrevID, RecName, RecMap, FSMState, MonitorState) ->
  IDRes = orddict:find(RecName, RecMap),
  case IDRes of
    {ok, ID} ->
      add_comm_transition(CommAST, PrevID, ID, FSMState, MonitorState);
    _ -> error(rec_name_unbound)
  end.

% Takes the ID of the previously-created node, end index, FSM state, monitor state.
% On each step?
% If final instruction in block and comm, make transition from PrevID to EndIndex.
% If final instruction in block and not comm, eval state as normal?
% If not final instruction in block, create new state, make transition from prev
% to new, predicated on the comm action, then recursively evaluate.
% Return FSMState and MonitorState.
%
evaluate_block([], _PrevIndex, _EndIndex, _RecMap, FSMState, MonitorState) ->
  {FSMState, MonitorState};
% Par is (unfortunately) a bit of a special case, and we need to take some care with it.
% Essentially, it should never be evaluated in this block.
% evaluate_block([{par, _}|_], _, _, _, _, _) -> error(eval_par);
evaluate_block([{continue, _}|_], _, _, _, FSMState, MonitorState) ->
  {FSMState, MonitorState};
evaluate_block([X], PrevIndex, EndIndex, RecMap, FSMState, MonitorState) ->
  IsCommAction = is_comm_action(X),
  if IsCommAction ->
       % Add transition to the end node of the scope
       add_comm_transition(X, PrevIndex, EndIndex, FSMState, MonitorState);
     not IsCommAction ->
       evaluate_scope(X, [], PrevIndex, EndIndex, RecMap, FSMState, MonitorState)
  end;
evaluate_block([X|XS], PrevIndex, EndIndex, RecMap, FSMState, MonitorState) ->
  IsCommAction = is_comm_action(X),
  RunningID = get_running_id(FSMState),
  if IsCommAction ->
       % Make a transition to PrevIndex, make a new state
       evaluate_transition(X, XS, PrevIndex, EndIndex, RecMap, FSMState, MonitorState);
     not IsCommAction ->
       ScopeEndIndex = RunningID + instruction_size(X),
       {FSMState1, MonitorState1} =
         evaluate_scope(X, XS, PrevIndex, ScopeEndIndex, RecMap, FSMState, MonitorState),
       FSMState2 = add_state(standard_state(), FSMState1),
       % PrevIndex here: end index of the scope.
       evaluate_block(XS, ScopeEndIndex, EndIndex, RecMap,
                      FSMState2, MonitorState1)
  end.


% Evaluates parallel blocks.
% Evaluates each block, stores in monitor state.
% Returns {MonitorState, Block ID List}.
evaluate_parallel_blocks(Blocks, MonitorState) ->
  evaluate_parallel_blocks_inner(Blocks, MonitorState, []).

evaluate_parallel_blocks_inner([], MonitorState, BlockIDs) ->
  {MonitorState, BlockIDs};
evaluate_parallel_blocks_inner([[]|ParallelBlocks], MonitorState, BlockIDs) ->
  evaluate_parallel_blocks_inner(ParallelBlocks, MonitorState, BlockIDs);
evaluate_parallel_blocks_inner([Block|ParallelBlocks], MonitorState, BlockIDs) ->
  FSMID = MonitorState#outer_monitor_gen_state.running_nested_fsm_id,
  EvalRes = evaluate_nested_fsm(Block, FSMID, MonitorState),
  case EvalRes of
    {ok, NewMonitorState} ->
      evaluate_parallel_blocks_inner(ParallelBlocks, NewMonitorState, [FSMID|BlockIDs]);
    Other -> Other
  end.

% Evaluates a list of blocks.
% Returns an FSMState and MonitorState, and a list of top IDs.
evaluate_blocks([], _PrevIndex, _EndIndex, _RecMap, FSMState, MonitorState) ->
  {FSMState, MonitorState};
evaluate_blocks([Block|Blocks], PrevIndex, EndIndex, RecMap, FSMState, MonitorState) ->
  {FSMState1, MonitorState1} = evaluate_block(Block, PrevIndex, EndIndex, RecMap, FSMState, MonitorState),
  evaluate_blocks(Blocks, PrevIndex, EndIndex, RecMap, FSMState1, MonitorState1).


evaluate_initiates(X = {local_initiates, _, _, _, Success, HBs},
                   PrevIndex, EndIndex, RecMap, FSMState, MonitorState) ->
  % Start subsession state
  InitiatesStateID = get_running_id(FSMState),
  {FSMState1, MonitorState1} =
    evaluate_comm_transition(X, PrevIndex, standard_state(), FSMState, MonitorState),
  % Now, we need states for all possible results of the subsession.
  % Evaluate the success block
  FSMState2 = add_subsession_success_transition(Success, RecMap, InitiatesStateID, EndIndex, FSMState1),
  SuccessStateID = get_running_id(FSMState1),
  {FSMState3, MonitorState2} =
    evaluate_block(Success, SuccessStateID, EndIndex, RecMap, FSMState2, MonitorState1),
  % Finally, evaluate all of the handler blocks.
  evaluate_handle_blocks(HBs, InitiatesStateID, EndIndex, RecMap, FSMState3, MonitorState2).


add_subsession_failure_transition(FailureName, [], _, PrevIndex, EndIndex, FSMState) ->
  Transition = subsession_failure_transition(FailureName, EndIndex),
  add_transition(PrevIndex, Transition, FSMState);
add_subsession_failure_transition(FailureName, [{continue, RecName}|_], RecMap, PrevIndex, _, FSMState) ->
  IDRes = orddict:find(RecName, RecMap),
  case IDRes of
    {ok, ID} ->
      Transition = subsession_failure_transition(ID, FailureName),
      add_transition(PrevIndex, Transition, FSMState);
    _ -> error(rec_name_unbound)
  end;
add_subsession_failure_transition(FailureName, _, _, PrevIndex, _, FSMState) ->
  RunningID = get_running_id(FSMState),
  FSMState1 = add_state(standard_state(), FSMState),
  Transition = subsession_failure_transition(RunningID, FailureName),
  add_transition(PrevIndex, Transition, FSMState1).



add_subsession_success_transition([], _, PrevIndex, EndIndex, FSMState) ->
  Transition = subsession_success_transition(EndIndex),
  add_transition(PrevIndex, Transition, FSMState);
add_subsession_success_transition([{continue, RecName}|_], RecMap, PrevIndex, _, FSMState) ->
  IDRes = orddict:find(RecName, RecMap),
  case IDRes of
    {ok, ID} ->
      Transition = subsession_success_transition(ID),
      add_transition(PrevIndex, Transition, FSMState);
    _ -> error(rec_name_unbound)
  end;
add_subsession_success_transition(_, _, PrevIndex, _, FSMState) ->
  RunningID = get_running_id(FSMState),
  FSMState1 = add_state(standard_state(), FSMState),
  Transition = subsession_success_transition(RunningID),
  add_transition(PrevIndex, Transition, FSMState1).

%add_transition(FromID, Transition, FSMState) ->


evaluate_handle_blocks([], _, _, _, FSMState, MonitorState) ->
  {FSMState, MonitorState};
evaluate_handle_blocks([HB|HBs], PrevIndex, EndIndex, RecMap, FSMState, MonitorState) ->
  RunningID = get_running_id(FSMState),
  {handle_block, FailureName, Interactions} = HB,
  FSMState1 = add_subsession_failure_transition(FailureName, Interactions, RecMap,
                                                PrevIndex, EndIndex, FSMState),
  {FSMState2, MonitorState1} =
    evaluate_block(Interactions, RunningID, EndIndex, RecMap, FSMState1, MonitorState),
  evaluate_handle_blocks(HBs, PrevIndex, EndIndex, RecMap, FSMState2, MonitorState1).


% Takes top-level scope AST, FSM state and monitor state.
% Returns ID of top node, new FSM state, new monitor state.
evaluate_scope(X, [{continue, RecName}|_], PrevID, _EndIndex,
                    RecMap, FSMState, MonitorState) ->
  RecID = orddict:fetch(RecName, RecMap),
  evaluate_scope_inner(X, PrevID, RecID, RecMap, FSMState, MonitorState);
evaluate_scope(X, _, PrevID, EndIndex, RecMap, FSMState, MonitorState) ->
  evaluate_scope_inner(X, PrevID, EndIndex, RecMap, FSMState, MonitorState).



evaluate_scope_inner({local_protocol, _, _, _, _, Block}, _PrevIndex, _EndIndex, RecMap,
               FSMState, MonitorState) ->
  ScopeEndIndex = instruction_size(Block),
  {FSMState1, MonitorState1} = evaluate_block(Block, 0, ScopeEndIndex, RecMap, FSMState, MonitorState),
  FSMState2 = add_state(standard_state(), FSMState1),
  {FSMState2, MonitorState1};
evaluate_scope_inner({rec, RecName, Interactions}, PrevIndex, EndIndex, RecMap, FSMState,
               MonitorState) ->
  NewRecMap = orddict:store(RecName, PrevIndex, RecMap),
  evaluate_block(Interactions, PrevIndex, EndIndex, NewRecMap, FSMState, MonitorState);
evaluate_scope_inner({choice, _, ChoiceBlocks}, PrevIndex, EndIndex,
               RecMap, FSMState, MonitorState) ->
  % We have a choice block.
  % We need to evaluate each block in turn.
  % Each block needs to converge on the end index.
  evaluate_blocks(ChoiceBlocks, PrevIndex, EndIndex, RecMap, FSMState, MonitorState);
evaluate_scope_inner(X = {local_initiates, _, _, _, _, _}, PrevIndex, EndIndex, RecMap,
                     FSMState, MonitorState) ->
  evaluate_initiates(X, PrevIndex, EndIndex, RecMap, FSMState, MonitorState).

