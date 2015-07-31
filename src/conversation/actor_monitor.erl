-module(actor_monitor).

-behaviour(gen_server2).
-compile(export_all).
-import(util, [list_contains/2]).
-record(monitor_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the attached actor type
                     active_protocols, % [{ProtocolName, RoleName, ConversationID}]
                     protocol_role_map, % Protocol Name |-> [Role]
                     registered_become_conversations, % Atom |-> Conversation
                     clean_monitors, % Clean monitors: (ProtocolName, RoleName) |-> Monitor
                     monitors, % (RoleName, ConversationID) |-> Monitor
                     routing_table, % ConversationID |-> [(Role, Monitor PID)]
                     queued_messages, % Message ID |-> Message
                     pending_calls, % Call ID |-> Monitor, post-send
                     pending_subsessions, % Subsession ID |-> Monitor, pre-start
                     failure_detection_strategy, % Either push or pull
                     participant_monitor_refs % {Conv ID, Role} |-> Monitor reference
                                              % Used for push-based detection.
                    }).

% New monitor state
% Say we can be involved in multiple protocols, and multiple roles within each
% protocol.
% In order to do this, we need each message to carry the conversation ID.
% active_protocols then becomes a map from protocol name |-> roles we're currently in
% protocol_role_map becomes a map from protocol name |-> roles we can take part in
% monitors becomes a map from (ProtocolName, RoleName, ConversationID) |-> Monitor

% New workflow:
% Always handle conversation invitation messages.
%  - Accept if we're not involved in a protocol of that type.
%  - Reject if we're not registered to partake in that protocol.
%
% Always handle conversation termination messages, removing from active
% conversation list.
%
% New architecture for actor_monitor: use a gen_server2 instead of a gen_fsm, as
% we no longer have to differentiate between the idle / setup / working states.
% As soon as we respond to the invitation message, we can partake in interactions
% for the registered conversation ID.

log_msg(Func, Format, Args, State) ->
  InfoStr = "Actor ~p, actor PID ~p, monitor instance ~p.",
  InfoArgs = [State#monitor_state.actor_type_name,
              State#monitor_state.actor_pid,
              self()],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

% Warn function. It was ad-hoc, horrific, and verbos before, so standardise it.
monitor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

% Error function. Same as warn, really
monitor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

monitor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).

fresh_state(ActorTypeName, ProtocolRoleMap, FailureDetectionStrategy) ->
  #monitor_state{
              actor_type_name=ActorTypeName, % Name of the attached actor type
              active_protocols=orddict:new(),
              protocol_role_map=ProtocolRoleMap, % Roles for each protocol
              registered_become_conversations=orddict:new(),
              monitors=orddict:new(),
              clean_monitors=orddict:new(),
              routing_table=orddict:new(),
              queued_messages=orddict:new(),
              pending_calls=orddict:new(),
              pending_subsessions=orddict:new(),
              failure_detection_strategy=FailureDetectionStrategy,
              participant_monitor_refs=orddict:new()
             }.

load_monitors([], MonitorDict, _) ->
  MonitorDict;
load_monitors([{ProtocolName, RoleNames}|XS], MonitorDict, State) ->
  NewMonitorDict = load_protocol_monitors(ProtocolName, RoleNames,
                                          MonitorDict, State),
  load_monitors(XS, NewMonitorDict, State).

load_protocol_monitors(_ProtocolName, [], MonitorDict, _State) -> MonitorDict;
load_protocol_monitors(ProtocolName, [Role|Roles], MonitorDict, State) ->
  %io:format("Getting monitor for protocol name: ~p, Role: ~p~n", [ProtocolName, Role]),
  MonitorRes = protocol_registry:get_monitor(ProtocolName, Role),
  case MonitorRes of
    {ok, {ok, Monitor}} ->
      NewDict = orddict:store({ProtocolName, Role}, Monitor, MonitorDict),
      load_protocol_monitors(ProtocolName, Roles, NewDict, State);
    Err ->
      monitor_warn("Could not load monitor for protocol ~s, role ~s: ~p ",
                   [ProtocolName, Role, Err], State),
      load_protocol_monitors(ProtocolName, Roles, MonitorDict, State)
  end.



init([{ActorTypeName, Args}]) ->
  ProtocolRoleMap = actor_type_registry:get_protocol_role_map(ActorTypeName),
  % Firstly, create a fresh state with all of the information we've been given
  % Next, we load the monitors.
  State = fresh_state(ActorTypeName, ProtocolRoleMap, pull), % TODO: Don't hardcode pull
  MonitorDict = load_monitors(orddict:to_list(ProtocolRoleMap),
                              orddict:new(),
                              State),
  {ok, ActorPID} = ssa_gen_server:start_actor_process(ActorTypeName, Args, self()),
  {ok, State#monitor_state{actor_pid=ActorPID, clean_monitors=MonitorDict}}.


% Called when we've been invited to fufil a role.
% If we can fulfil the role and haven't already fulfilled the role, then
% load the empty FSM into the monitors list.
% Returns {ok, NewState} if we can fulfil the role, or either:
%   * {error, already_fulfilled} --> if we've already fulfilled the role
%   * {error, cannot_fulfil} --> if this role isn't offered by the actor
add_role(ProtocolName, RoleName, ConversationID, State) ->
  % Firstly, check to see we're registered for the role
  ProtocolRoleMap = State#monitor_state.protocol_role_map,
  ActiveProtocols = State#monitor_state.active_protocols,
  Monitors = State#monitor_state.monitors,
  FreshMonitors = State#monitor_state.clean_monitors,
  ActorPID = State#monitor_state.actor_pid,
  % First, check whether we can fulfil the role (ie it's contained in the PRM)
  RoleFindRes =
    case orddict:find(ProtocolName, ProtocolRoleMap) of
      {ok, ProtocolRoles} ->
        list_contains(RoleName, ProtocolRoles);
      error -> false
    end,

  FreshMonitorRes =
    orddict:find({ProtocolName, RoleName}, FreshMonitors),

    % Check to see whether we already have an entry for the protocol-role-cid
    % triple. If not, then we can fulfil it.
    AlreadyFulfilled =
    lists:any(fun(ActiveTuple) ->
                  ActiveTuple == {ProtocolName, {RoleName, ConversationID}} end,
              orddict:to_list(ActiveProtocols)),

    case {RoleFindRes, AlreadyFulfilled, FreshMonitorRes} of
    {true, false, {ok, FreshMonitor}} ->
      % We can theoretically fulfil it. Just need to ask the actor...
      JoinRequestResult = ssa_gen_server:join_conversation_request(ActorPID, ProtocolName,
                                                                   RoleName, ConversationID),
      case JoinRequestResult of
        accept ->
          NewActiveProtocols = orddict:append(ProtocolName, {RoleName, ConversationID}, ActiveProtocols),
          NewMonitors = orddict:store({ConversationID, RoleName},
                                      FreshMonitor, Monitors),

          Res = conversation_instance:accept_invitation(ConversationID, RoleName, self()),
          case Res of
            % Now, add the <CID, Role> |-> Monitor mapping
            ok -> {ok, State#monitor_state{active_protocols=NewActiveProtocols,
                                        monitors=NewMonitors}};
            {error, Err} -> {error, Err}
          end;
        decline ->
          {error, actor_declined}
      end;
    {_, true, _} -> {error, already_fulfilled};
    _Other -> {error, cannot_fulfil}
  end.

% Handle a request from the server, checking whether or not the role is needed
% from the point of view of this participant in the rest of the protocol
handle_check_participant(ConvID, RoleName, State) ->
  Monitors = State#monitor_state.monitors,
  MonitorRes = orddict:find({ConvID, RoleName}, Monitors),
  case MonitorRes of
    {ok, MonitorInstance} ->
      monitor:is_role_reachable(RoleName, MonitorInstance);
    error ->
      % Role's not involved in this local protocol at all
      false
  end.


% Handles an invitation to fulfil a role
handle_invitation(ProtocolName, RoleName, ConversationID, State) ->
  AddRoleResult = add_role(ProtocolName, RoleName, ConversationID, State),
  % Try and add the role.
  % If we succeed, add the role to the conversation_roles list, set the
  % conversation ID, and transition to setup.
  % If not (eg we can't fulfil the role), then we stay where we are, and make
  % no changes to the state or state data.
  case AddRoleResult of
    {ok, NewState} ->
      monitor_info("Registered for role ~s in protocol ~s.",
                   [RoleName, ProtocolName], State),
      {reply, ok, NewState};
    {error, Err} ->
      monitor_warn("Could not fulfil role ~s in protocol ~s. Error: ~p",
                   [RoleName, ProtocolName, Err], State),
      {reply, {error, Err}, State}
  end.

get_monitor(CID, Role, State) ->
  Monitors = State#monitor_state.monitors,
  orddict:fetch({CID, Role}, Monitors).

update_monitor(CID, Role, Monitor, State) ->
  Monitors = State#monitor_state.monitors,
  NewMonitors = orddict:store({CID, Role}, Monitor, Monitors),
  State#monitor_state{monitors=NewMonitors}.

handle_subsession_ended(success, SubsessionName, InitiatorPN, InitiatorRN,
                          InitiatorCID, Result, State) ->
  ActorPID = State#monitor_state.actor_pid,
  Monitors = State#monitor_state.monitors,
  % Check against & advance the monitor
  Monitor = get_monitor(InitiatorCID, InitiatorRN, State),
  MonitorRes = monitor:subsession_success(Monitor),
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      ssa_gen_server:subsession_success(ActorPID, SubsessionName, InitiatorPN,
                                        InitiatorRN, InitiatorCID, Result),
      {noreply, update_monitor(InitiatorCID, InitiatorRN, NewMonitorInstance, State)};
    _Other ->
      % Unexpected success -- internal error, this should never happen
      exit(unexpected_success),
      {noreply, State}
  end;
handle_subsession_ended(failure, SubsessionName, InitiatorPN, InitiatorRN,
                          InitiatorCID, FailureName, State) ->
  ActorPID = State#monitor_state.actor_pid,
  % Check against & advance the monitor
  Monitor = get_monitor(InitiatorCID, InitiatorRN, State),
  MonitorRes = monitor:subsession_failure(FailureName, Monitor),
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      ssa_gen_server:subsession_failure(ActorPID, SubsessionName, InitiatorPN,
                                        InitiatorRN, InitiatorCID, FailureName),
      {noreply, update_monitor(InitiatorCID, InitiatorRN, NewMonitorInstance, State)};
    _Other ->
      % Unhandled exception -- crash
      % To revisit, maybe: kill session instead of actor?
      exit({unhandled_failure, FailureName}),
      {noreply, State}
  end.

% Here, we deliver the message to the attached actor (which is a gen_server2).
deliver_incoming_message(Protocol, Role, ConvID, Msg, State) ->
  RecipientPID = State#monitor_state.actor_pid,
  gen_server2:cast(RecipientPID, {ssa_msg, Protocol, Role, ConvID, Msg}).


%%%%%%% OUTGOING MESSAGE DELIVERY %%%%%%%%

%deliver_outgoing_message(Msg, ProtocolName, RoleName, ConversationID) ->
  %gen_server2:call(ConversationID, {outgoing_msg, ProtocolName, RoleName, Msg}).


% Dispatch messages to each actor.
% Return a tuple of four things:
%   * OkNodes: Nodes for which the message was successfully delivered
%   * DeadNodes: Nodes which are in the role map but not active
%   * RejectedNodes: Nodes for which the monitor rejected the message
%   * NotFoundNodes: Nodes which were not found in the role map.
% return: [{AliveNodeEndpoints, DeadNodes, RejectedNodes, NotFoundNodes}]
deliver_messages(Roles, RoleMap, P, R, C, Msg) ->
  deliver_messages_inner(Roles, RoleMap, P, R, C, Msg, {[], [], [], []}).
deliver_messages_inner([], _RoleMap, _, _, _, _, Res) -> Res;
deliver_messages_inner([Role|Roles], RoleMap, Protocol, RoleName, ConvID, Msg, {Ok, D, R, NF}) ->
  case orddict:find(Role, RoleMap) of
    {ok, Endpoint} ->
      % TODO: Reflexive sending will deadlock here.
      % Instead of sending to ourselves, we could probably emulate it and return
      % an updated state, while adding the message to the queue, ready for the
      % commit cast later on.
      try actor_monitor:queue_message(Endpoint, Protocol, Role, ConvID, Msg) of
        ok ->
          % Delivered successfully, add to Ok list
          deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                            Msg, {[Endpoint|Ok], D, R, NF});
        {error, _Err} ->
          % Node alive, but monitor rejected message, add to rejected list
          deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                            Msg, {Ok, D, [Role|R], NF})
      catch
        _:Err ->
          %io:format("Error in queue message call: ~p~n", [Err]),
          % Actor offline, add to Dead list
          deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                            Msg, {Ok, [Role|D], R, NF})
      end;
    error ->
      % Role not found -- user entered it incorrectly. Add to not found
      deliver_messages_inner(Role, RoleMap, Protocol, RoleName, ConvID,
                        Msg, {Ok, D, R, [Role|NF]})
  end.

% Commit a message to a set of endpoints
commit_outgoing_messages([], _) -> ok;
commit_outgoing_messages([EP|EPs], Msg) ->
  actor_monitor:commit_message(EP, message:message_id(Msg)),
  commit_outgoing_messages(EPs, Msg).

% Drop a message from a set of endpoints
drop_outgoing_messages([], _) -> ok;
drop_outgoing_messages([EP|EPs], Msg) ->
  actor_monitor:drop_message(EP, message:message_id(Msg)),
  drop_outgoing_messages(EPs, Msg).


direct_deliver_outgoing_message(Recipient, ConvRoutingTable, ProtocolName,
                                ConvID, Msg, IsNoErr) ->
  case orddict:find(Recipient, ConvRoutingTable) of
    {ok, Endpoint} ->
      %actor_monitor:direct_deliver_message(Endpoint, ProtocolName, Recipient, ConvID, Msg);
      if IsNoErr ->
           actor_monitor:direct_deliver_message_noerr(Endpoint, ProtocolName, Recipient, ConvID, Msg);
        not IsNoErr ->
           actor_monitor:direct_deliver_message(Endpoint, ProtocolName, Recipient, ConvID, Msg)
      end;
    error -> {error, {nonexistent_role, Recipient}}
  end.

% Lookup the destination role, and forward to its monitor.
deliver_outgoing_message(ProtocolName, RoleName, ConvID, Msg, IsNoErr, State) ->
  %io:format("Message data: ~p~n", [Msg]),
  Recipients = message:message_recipients(Msg),
  GlobalRoutingTable = State#monitor_state.routing_table,
  ConvRoutingTable = orddict:fetch(ConvID, GlobalRoutingTable),
  % Optimisation: if there's only the one participant, we don't have to
  % do any queueing.
  RecipientLength = length(Recipients),
  if RecipientLength == 1 ->
       [Recipient] = Recipients,
       direct_deliver_outgoing_message(Recipient, ConvRoutingTable, ProtocolName,
                                       ConvID, Msg, IsNoErr);
     RecipientLength =/= 1 ->
       MonitorAndQueueRes = deliver_messages(Recipients, ConvRoutingTable,
                                             ProtocolName, RoleName, ConvID, Msg),
       case MonitorAndQueueRes of
         {OkEndpoints, [], [], []} ->
           % Everything was successful
           commit_outgoing_messages(OkEndpoints, Msg),
           ok;
         {OkEndpoints, D, R, NF} ->
           drop_outgoing_messages(OkEndpoints, Msg),
           {error, {queue_failed, D, R, NF}}
       end
  end.


% Handles an incoming message. Checks whether we're in the correct conversation,
% then grabs the monitor, then checks / updates the monitor state.
handle_incoming_message(MessageData, ProtocolName, RoleName, ConversationID,
                        State) ->
  %monitor_info("Handling incoming message ~p", [MessageData], State),
  Res = monitor_msg(recv, MessageData, ProtocolName, RoleName, ConversationID, State),
  case Res of
    {ok, NewState} ->
      deliver_incoming_message(ProtocolName, RoleName, ConversationID,
                               MessageData, NewState),
      {noreply, NewState};
    _Err -> {noreply, State} % assuming the error has been logged already
  end.

filter_orddict(PredFun, Orddict) ->
  orddict:from_list(lists:filter(PredFun, orddict:to_list(Orddict))).

filter_active_protocols(ActiveProtocolsDict, ConvID) ->
  ActiveProtocolsList = orddict:to_list(ActiveProtocolsDict),
  FilteredList =
    lists:map(fun({PN, RoleCIDTuples}) ->
                  FilteredTuples = lists:filter(fun({_R, CID}) -> CID =/= ConvID end,
                                                RoleCIDTuples),
                  {PN, FilteredTuples} end, ActiveProtocolsList),
  orddict:to_list(FilteredList).

handle_conversation_ended(ConversationID, Reason, State) ->
  ActorPID = State#monitor_state.actor_pid,
  {ActiveProtocols, BecomeConvs, Monitors} = {State#monitor_state.active_protocols,
                                              State#monitor_state.registered_become_conversations,
                                              State#monitor_state.monitors},
  NewActiveProtocols = filter_active_protocols(ActiveProtocols, ConversationID),
  NewMonitors = filter_orddict(fun({{CID, _Role}, _}) -> CID =/= ConversationID end,
                              Monitors),
  NewBecomeConversations = filter_orddict(fun({_, CID}) -> CID =/= ConversationID end,
                                          BecomeConvs),
  RoutingTable = State#monitor_state.routing_table,
  NewRoutingTable = filter_orddict(fun({CID, _}) -> CID =/= ConversationID end,
                                   RoutingTable),
  NewState = State#monitor_state{active_protocols=NewActiveProtocols,
                                 registered_become_conversations=NewBecomeConversations,
                                 monitors=NewMonitors,
                                 routing_table=NewRoutingTable
                                },
  ssa_gen_server:conversation_ended(ActorPID, ConversationID, Reason),
  NewState.

handle_register_conv(ProtocolName, _RoleName, ConversationID, RegAtom, State) ->
  RegisteredConversations = State#monitor_state.registered_become_conversations,
  NewRegisteredConversations = orddict:store(RegAtom, {ProtocolName, ConversationID}, RegisteredConversations),
  NewState = State#monitor_state{registered_become_conversations = NewRegisteredConversations},
  {reply, ok, NewState}.


handle_become(RegAtom, RoleName, Operation, Arguments, State) ->
  RecipientPID = State#monitor_state.actor_pid,
  RegisteredConversations = State#monitor_state.registered_become_conversations,
  CIDRes = orddict:find(RegAtom, RegisteredConversations),
  case CIDRes of
    {ok, {ProtocolName, CID}} ->
      gen_server2:cast(RecipientPID, {become, ProtocolName, RoleName, Operation, Arguments, CID}),
      {reply, ok, State};
    _ ->
      {reply, error, bad_conversation}
  end.

handle_outgoing_message(ProtocolName, RoleName, ConversationID, Recipients,
                        MessageName, Types, Payload, IsNoErr, State) ->
  % Construct a message instance, send to the monitor, and check the result
  MessageData = message:message(make_ref(), RoleName, Recipients,
                                MessageName, Types, Payload),
  deliver_outgoing_message(ProtocolName, RoleName, ConversationID,
                           MessageData, IsNoErr, State),
  {ok, State}.

 %MonitorRes = monitor_msg(send, MessageData, ProtocolName, RoleName,
 %                         ConversationID, State),
 %case MonitorRes of
 %    {ok, NewMonitorInstance} ->
 %      Monitors = State#monitor_state.monitors,
 %      NewMonitors = orddict:store({ConversationID, RoleName},
 %                                  NewMonitorInstance,
 %                                  Monitors),
 %      NewState = State#monitor_state{monitors=NewMonitors},
 %      OutgoingRes =
 %        deliver_outgoing_message(ProtocolName, RoleName, ConversationID,
 %                                 MessageData, IsNoErr, NewState),
 %      % Update monitors only if sending was successful
 %      NewState1 =
 %        if OutgoingRes == ok -> NewState;
 %           OutgoingRes =/= ok -> State
 %        end,

 %      if OutgoingRes =/= ok ->
 %           handle_send_failed(ConversationID, RoleName, State);
 %         OutgoingRes == ok -> ok
 %      end,
 %      {OutgoingRes, NewState1};
 %    Err ->
 %      {Err, State}
 %end.

handle_send_failed(ConvID, RoleName, State) ->
  error_logger:error_msg("Could not reach ~p, terminating conversation~n", [RoleName]),
  conversation_instance:end_conversation(ConvID, {role_offline, RoleName}).

monitor_msg(CommType, MessageData, _ProtocolName, RoleName, ConversationID, State) ->
  Monitors = State#monitor_state.monitors,
  Monitor = orddict:fetch({ConversationID, RoleName}, Monitors),
  monitor_with(CommType, MessageData, Monitor, State).

monitor_with(CommType, MessageData, Monitor, State) ->
  MonitorResult =
    case CommType of
        send -> monitor:send(MessageData, Monitor);
        recv -> monitor:recv(MessageData, Monitor);
        send_call_req -> monitor:send_call_request(MessageData, Monitor);
        recv_call_req -> monitor:recv_call_request(MessageData, Monitor);
        send_call_resp -> monitor:send_call_response(MessageData, Monitor);
        recv_call_resp -> monitor:recv_call_response(MessageData, Monitor)
    end,
  case MonitorResult of
    {ok, NewMonitorInstance} ->
      {ok, NewMonitorInstance};
    {error, Err} ->
      monitor_warn("Monitor failed when processing message ~p (~p). Error: ~p~n",
                   [MessageData, CommType, Err], State),
      {error, Err}
  end.



handle_send_delayed_invite(ProtocolName, RoleName, ConversationID, InviteeMonitorPid,
                           State) ->
    InviteRes = protocol_registry:invite_actor_direct(ProtocolName,
                                                      ConversationID,
                                                      RoleName,
                                                      InviteeMonitorPid),
    case InviteRes of
      {ok, ok} -> {reply, ok, State};
      % Found protocol, but error in invitation
      {ok, Err} -> {reply, Err, State};
      % Couldn't find protocol
      Err -> {reply, Err, State}
    end.

monitor_and_deliver_noerr(ProtocolName, RoleName, ConvID, Msg, State) ->
  ActorPID = State#monitor_state.actor_pid,
  ssa_gen_server:message(ActorPID, ProtocolName, RoleName, ConvID, Msg),
  State.
 %MonitorRes = monitor_msg(recv, Msg, ProtocolName, RoleName, ConvID, State),
 %case MonitorRes of
 %  {ok, NewMonitorInstance} ->
 %    Monitors = State#monitor_state.monitors,
 %    NewMonitors = orddict:store({ConvID, RoleName},
 %                                 NewMonitorInstance, Monitors),
 %    NewState = State#monitor_state{monitors=NewMonitors},
 %    ActorPID = State#monitor_state.actor_pid,
 %    ssa_gen_server:message(ActorPID, ProtocolName, RoleName, ConvID, Msg),
 %    NewState;
 %  {error, Err} -> State
 %end.

monitor_and_deliver(ProtocolName, RoleName, ConvID, Msg, State) ->
  MonitorRes = monitor_msg(recv, Msg, ProtocolName, RoleName, ConvID, State),
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      Monitors = State#monitor_state.monitors,
      NewMonitors = orddict:store({ConvID, RoleName},
                                   NewMonitorInstance, Monitors),
      NewState = State#monitor_state{monitors=NewMonitors},
      ActorPID = State#monitor_state.actor_pid,
      ssa_gen_server:message(ActorPID, ProtocolName, RoleName, ConvID, Msg),
      {ok, NewState};
    {error, Err} -> {{error, Err}, State}
  end.

monitor_and_queue(ProtocolName, RoleName, ConvID, Msg, State) ->
  MsgRef = message:message_id(Msg),
  MonitorRes = monitor_msg(recv, Msg, ProtocolName, RoleName, ConvID, State),
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      QueuedMsgs = State#monitor_state.queued_messages,
      NewQueuedMsgs = orddict:store(MsgRef, {ProtocolName, RoleName,
                                             ConvID, Msg, NewMonitorInstance}, QueuedMsgs),
      NewState = State#monitor_state{queued_messages=NewQueuedMsgs},
      {ok, NewState};
    {error, Err} -> {{error, Err}, State}
  end.

handle_commit(MsgRef, State) ->
  % Deliver the message, remove from the queue, and advance the monitor
  ActorPid = State#monitor_state.actor_pid,
  QueuedMsgs = State#monitor_state.queued_messages,
  Monitors = State#monitor_state.monitors,
  {ProtocolName, RoleName, ConvID, Msg, NewMonitor} = orddict:fetch(MsgRef, QueuedMsgs),
  ssa_gen_server:message(ActorPid, ProtocolName, RoleName, ConvID, Msg),
  NewQueuedMsgs = orddict:erase(MsgRef, QueuedMsgs),
  NewMonitors = orddict:store({ConvID, RoleName},
                             NewMonitor, Monitors),
  State#monitor_state{queued_messages=NewQueuedMsgs, monitors=NewMonitors}.

handle_send_call_req(ProtocolName, RoleName, ConvID, Recipient, MessageName,
                     Payload, From, State) ->
  Message = message:message(make_ref(), RoleName, [Recipient], MessageName,
                            [], Payload),
  MonitorRes = monitor_msg(send_call_req, Message, ProtocolName, RoleName, ConvID, State),
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      % Store the new monitor instance in case the call fails for some reason
      % Send the call request
      GlobalRoutingTable = State#monitor_state.routing_table,
      ConvRoutingTable = orddict:fetch(ConvID, GlobalRoutingTable),
      case orddict:find(Recipient, ConvRoutingTable) of
        {ok, Endpoint} ->
          % This is a call, and checks whether the receive monitor accepts the call request
          ReqCheckRes = actor_monitor:incoming_call_request(Endpoint,
                                                            ProtocolName,
                                                            RoleName, ConvID,
                                                            Message, From),

          case ReqCheckRes of
            ok ->
              % It was accepted, now we can just wait for the response.
              PendingCalls = State#monitor_state.pending_calls,
              NewPendingCalls = orddict:store(From, NewMonitorInstance, PendingCalls),
              NewState = State#monitor_state{pending_calls=NewPendingCalls},
              {noreply, NewState};
            Err ->
              % It was not accepted -- return an error
              {reply, {call_rejected, Err}, State}
          end
      end;
    Err ->
      {reply, Err, State}
  end.

handle_incoming_call_req(ProtocolName, _SenderName, ConvID, Message, From, State) ->
  ActorPID = State#monitor_state.actor_pid,
  [RoleName] = message:message_recipients(Message),
  % First, check that we can receive it
  case monitor_msg(recv_call_req, Message, ProtocolName, RoleName, ConvID, State) of
    {ok, NewMonitorInstance} ->
      % Delegate to actor, store new monitor.
      % The actor will return the response asynchronously once everything's processed.
      ssa_gen_server:incoming_call_request(ActorPID, ProtocolName, RoleName, ConvID, self(), Message, From),
      Monitors = State#monitor_state.monitors,
      NewMonitors = orddict:store({ConvID, RoleName}, NewMonitorInstance, Monitors),
      NewState = State#monitor_state{monitors=NewMonitors},
      {reply, ok, NewState};
    Err ->
      {reply, Err, State}
  end.

handle_outgoing_call_resp(ProtocolName, RoleName, ConvID, Recipient, MessageName,
                          Payload, From, State) ->
  Message = message:message(make_ref(), RoleName, [Recipient], MessageName,
                            [], Payload),
  MonitorRes = monitor_msg(send_call_resp, Message, ProtocolName, RoleName, ConvID, State),
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      GlobalRoutingTable = State#monitor_state.routing_table,
      ConvRoutingTable = orddict:fetch(ConvID, GlobalRoutingTable),
      case orddict:find(Recipient, ConvRoutingTable) of
        {ok, Endpoint} ->
          % Send it back
          RespCheckRes = actor_monitor:incoming_call_response(Endpoint,
                                                             ProtocolName,
                                                             RoleName, ConvID,
                                                             Message, From),
          case RespCheckRes of
            ok ->
              Monitors = State#monitor_state.monitors,
              NewMonitors = orddict:store({ConvID, RoleName}, NewMonitorInstance, Monitors),
              NewState = State#monitor_state{monitors=NewMonitors},
              {reply, ok, NewState};
            Err ->
              % It was not accepted -- return an error, let it be tried again?
              {reply, {call_rejected, Err}, State}
          end
      end;
    Err ->
      {reply, Err, State}
  end.


handle_incoming_call_resp(_ProtocolName, RoleName, ConvID, Message, From, State) ->
  Response = message:payload(Message),
  PendingCalls = State#monitor_state.pending_calls,
  Monitor = orddict:fetch(From, PendingCalls),
  NewPendingCalls = orddict:erase(From, PendingCalls),
  case monitor_with(recv_call_resp, Message, Monitor, State) of
    {ok, NewMonitorInstance} ->
      % Send back to the patiently-waiting original caller
      gen_server2:reply(From, Response),
      Monitors = State#monitor_state.monitors,
      NewMonitors = orddict:store({ConvID, RoleName}, NewMonitorInstance, Monitors),
      NewState = State#monitor_state{pending_calls=NewPendingCalls, monitors=NewMonitors},
      {reply, ok, NewState};
    Err ->
      {reply, Err, State}
  end.

handle_check_role_reachable(ConvID, LocalRoleName, TargetRoleName, State) ->
  Monitors = State#monitor_state.monitors,
  Monitor = orddict:fetch({ConvID, LocalRoleName}, Monitors),
  monitor:is_role_reachable(TargetRoleName, Monitor).

% {ProtocolName, RoleName, ConversationID, MonitorPID},
% TRIANGLE OF DOOOOOOOOOOOOOOOOOOOM
% I miss monads so much
handle_start_subsession(ConvKey, SubsessionName, InternalInvitations, ExternalInvitations, State) ->
  {InitiatorPN, InitiatorRN, InitiatorCID, _} = ConvKey,
  MonitorPID = self(),
  % Check against the initiator's monitor.
  Monitors = State#monitor_state.monitors,
  MonitorInstance = orddict:fetch({InitiatorCID, InitiatorRN}, Monitors),
  MonitorRes = monitor:start_subsession(SubsessionName, InternalInvitations,
                                        ExternalInvitations, MonitorInstance),
  PendingSubsessions = State#monitor_state.pending_subsessions,
  case MonitorRes of
    {ok, NewMonitorInstance} ->
      RoleRes = protocol_registry:get_roles(SubsessionName),
      case RoleRes of
        {ok, RoleSpecs} ->
          % Start the subsession
          SubsessionProcRes = conversation_instance:start_subsession(SubsessionName, RoleSpecs,
                                                                     InitiatorCID, MonitorPID,
                                                                     InitiatorPN, InitiatorRN),
          case SubsessionProcRes of
            {ok, SubsessionPID} ->
              % Great, store old monitor instance, start invitations, and get on
              % with our day
              %NewPendingSubsessions = orddict:store(SubsessionPID,
              %                                      {ConvKey, MonitorInstance}),
              NewPendingSubsessions = orddict:store(SubsessionPID, MonitorInstance, PendingSubsessions),
              Monitors = State#monitor_state.monitors,
              NewMonitors = orddict:store({InitiatorCID, InitiatorRN}, NewMonitorInstance, Monitors),
              NewState = State#monitor_state{pending_subsessions=NewPendingSubsessions,
                                             monitors=NewMonitors},
              conversation_instance:start_subsession_invitations(SubsessionPID, InternalInvitations,
                                                                 ExternalInvitations),
              {noreply, NewState};
            % Couldn't start the subsesion process
            error ->
              subsession_setup_failed(MonitorPID, SubsessionName, undefined,
                                      InitiatorPN, InitiatorRN, InitiatorCID,
                                      bad_subsesion_proc),
              {noreply, State}
          end;
        % Couldn't find RoleSpec: bad protocol
        _Err ->
          subsession_setup_failed(MonitorPID, SubsessionName, undefined,
                                  InitiatorPN, InitiatorRN, InitiatorCID,
                                  bad_protocol),
          {noreply, State}
      end;
    % Monitor failed
    _Err ->
      subsession_setup_failed(MonitorPID, SubsessionName, undefined,
                              InitiatorPN, InitiatorRN, InitiatorCID,
                              bad_action),
      {noreply, State}
  end.



handle_subsession_setup_failed(SubsessionName, SubsessionPID,
                               InitiatorPN, InitiatorRN, InitiatorCID, Reason, State) ->
  ActorPID = State#monitor_state.actor_pid,
  State1 =
    if SubsessionPID =/= undefined ->
         % We need to restore previous monitor state here
         PendingSubsessions = State#monitor_state.pending_subsessions,
         Monitors = State#monitor_state.monitors,
         ReplacementMonitorInstance = orddict:fetch(SubsessionPID, PendingSubsessions),
         PendingSubsessions1 = orddict:erase(SubsessionPID, PendingSubsessions),
         Monitors1 = orddict:store({InitiatorCID, InitiatorRN}, ReplacementMonitorInstance,
                                  Monitors),

         State#monitor_state{pending_subsessions=PendingSubsessions1,
                             monitors=Monitors1};
       SubsessionPID == undefined -> State
    end,
  ssa_gen_server:subsession_setup_failure(ActorPID, SubsessionName,
                                          InitiatorPN, InitiatorRN, InitiatorCID, Reason),
  {noreply, State1}.

handle_subsession_ended(_SubsessionPID, _State) ->
  % Advance monitor.
  ok.


% Synchronous messages:
%  * Invitation
%  * Termination
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _Sender, State) ->
  handle_invitation(ProtocolName, RoleName, ConversationID, State);
handle_call({send_msg, CurrentProtocol, CurrentRole, ConversationID, Recipients,
             MessageName, Types, Payload}, _Sender, State) ->
  {Reply, NewState} =
    handle_outgoing_message(CurrentProtocol, CurrentRole, ConversationID, Recipients,
                            MessageName, Types, Payload, false, State),
  {reply, Reply, NewState};
handle_call({direct_deliver_msg, ProtocolName, RoleName, ConvID, Msg}, _, State) ->
  {Res, NewState} = monitor_and_deliver(ProtocolName, RoleName, ConvID, Msg, State),
  {reply, Res, NewState};
handle_call({queue_msg, ProtocolName, RoleName, ConvID, Msg}, _From, State) ->
  %io:format("In queue msg handler~n"),
  {Res, NewState} = monitor_and_queue(ProtocolName, RoleName, ConvID, Msg, State),
  {reply, Res, NewState};
handle_call({become, RoleName, RegAtom, Operation, Arguments}, _Sender, State) ->
  handle_become(RegAtom, RoleName, Operation, Arguments, State);
handle_call({send_delayed_invite, ProtocolName, InviteeRoleName, ConversationID, InviteeMonitorPid},
            _Sender, State) ->
  handle_send_delayed_invite(ProtocolName, InviteeRoleName, ConversationID, InviteeMonitorPid, State);
% Delegate directly to handle_call in monitor
handle_call({register_become, RegAtom, ProtocolName, RoleName, ConvID}, _From, State) ->
  handle_register_conv(ProtocolName, RoleName, ConvID, RegAtom, State);
% Send a call request
handle_call({make_call, ProtocolName, RoleName, ConvID, Recipient,
                         MessageName, Payload}, From, State) ->
  handle_send_call_req(ProtocolName, RoleName, ConvID, Recipient, MessageName, Payload, From, State);
handle_call({incoming_call_req, ProtocolName, RoleName, ConvID, Message, From}, _, State) ->
  handle_incoming_call_req(ProtocolName, RoleName, ConvID, Message, From, State);

handle_call({outgoing_call_resp, ProtocolName, RoleName, ConvID, Recipient,
             MessageName, Payload, From}, _, State) ->
  handle_outgoing_call_resp(ProtocolName, RoleName, ConvID, Recipient, MessageName,
                            Payload, From, State);
handle_call({incoming_call_resp, ProtocolName, RoleName, ConvID, Message, From}, _, State) ->
  handle_incoming_call_resp(ProtocolName, RoleName, ConvID, Message, From, State);
handle_call({check_role_reachable, ConvID, LocalRoleName, TargetRoleName}, _From, State) ->
  Res = handle_check_role_reachable(ConvID, LocalRoleName, TargetRoleName, State),
  {reply, Res, State};
handle_call(ping, _, State) ->
  {reply, pong, State};
handle_call(Msg, From, State) ->
  %io:format("Pass-through call: ~p~n", [Msg]),
  ActorPid = State#monitor_state.actor_pid,
  Reply = gen_server2:call(ActorPid, {delegate_call, From, Msg}),
  {reply, Reply, State}.

% Module:handle_cast(Request, State) -> Result
% Only async messages are actually data ones.
% Delivering these, we'll need a conv ID, I think.
handle_cast({message, ProtocolName, RoleName, ConversationID, MessageData}, State) ->
  handle_incoming_message(MessageData, ProtocolName, RoleName, ConversationID, State);
handle_cast({commit_msg, MsgRef}, State) ->
  NewState = handle_commit(MsgRef, State),
  {noreply, NewState};
handle_cast({drop_msg, MsgRef}, State) ->
  QueuedMsgs = State#monitor_state.queued_messages,
  NewQueuedMsgs = orddict:erase(MsgRef, QueuedMsgs),
  {noreply, State#monitor_state{queued_messages=NewQueuedMsgs}};
handle_cast({ssa_session_established, ProtocolName, RoleName, ConvID, RoutingTable}, State) ->
  % Set the routing table for the session, and then forward the success message.
  %io:format("In ssa session est ~n"),
  ActorPID = State#monitor_state.actor_pid,
  GlobalRoutingTable = State#monitor_state.routing_table,
  NewGlobalRoutingTable = orddict:store(ConvID, RoutingTable, GlobalRoutingTable),
  NewState = State#monitor_state{routing_table=NewGlobalRoutingTable},
  ssa_gen_server:conversation_success(ActorPID, ProtocolName, RoleName, ConvID),
  {noreply, NewState};
handle_cast({conversation_ended, CID, Reason}, State) ->
  NewState = handle_conversation_ended(CID, Reason, State),
  {noreply, NewState};
handle_cast({subsession_setup_failed, SubsessionName, SubsessionPID, InitiatorPN,
             InitiatorRN, InitiatorCID, Reason}, State) ->
  handle_subsession_setup_failed(SubsessionName, SubsessionPID, InitiatorPN, InitiatorRN,
                                 InitiatorCID, Reason, State);
handle_cast({start_subsession, ConvKey, ProtocolName, InternalInvitations,
             ExternalInvitations}, State) ->
  handle_start_subsession(ConvKey, ProtocolName, InternalInvitations, ExternalInvitations,
                          State);
handle_cast({subsession_success, SubsessionName, InitiatorPN, InitiatorRN,
             InitiatorCID, Result}, State) ->
  handle_subsession_ended(success, SubsessionName, InitiatorPN, InitiatorRN,
                          InitiatorCID, Result, State);
handle_cast({subsession_failure, SubsessionName, InitiatorPN, InitiatorRN,
             InitiatorCID, Result}, State) ->
  handle_subsession_ended(failure, SubsessionName, InitiatorPN, InitiatorRN,
                          InitiatorCID, Result, State);
handle_cast({direct_deliver_msg_noerr, ProtocolName, RoleName, ConvID, Msg}, State) ->
  NewState = monitor_and_deliver_noerr(ProtocolName, RoleName, ConvID, Msg, State),
  {noreply, NewState};
handle_cast({send_msg_noerr, CurrentProtocol, CurrentRole, ConversationID, Recipients,
             MessageName, Types, Payload}, State) ->
  {_, NewState} =
    handle_outgoing_message(CurrentProtocol, CurrentRole, ConversationID, Recipients,
                            MessageName, Types, Payload, true, State),
  {noreply, NewState};
handle_cast(Other, State) ->
  %monitor_warn("Received unhandled cast message ~p.", [Other], State),
  ActorPid = State#monitor_state.actor_pid,
  gen_server2:cast(ActorPid, Other),
  {noreply, State}.

handle_info(Info, State) ->
  monitor_warn("Received unhandled info message ~p.", [Info], State),
  ActorPid = State#monitor_state.actor_pid,
  ActorPid ! Info,
  {noreply, State}.

terminate(Reason, State) ->
  monitor_error("Terminated for reason ~p.", [Reason], State),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.


%%%%%%%%%%%%%%%%%%%
%% API Functions %%
%%%%%%%%%%%%%%%%%%%

check_role_reachable(MonitorPID, ConvID, LocalRoleName, TargetRoleName) ->
  gen_server2:call(MonitorPID, {check_role_reachable, ConvID, LocalRoleName, TargetRoleName}).

deliver_message(MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:cast(MonitorPID, {message, ProtocolName, RoleName, ConvID, Msg}).

register_become(MonitorPID, RegAtom, ProtocolName, RoleName, ConvID) ->
  gen_server2:call(MonitorPID, {register_become, RegAtom, ProtocolName, RoleName, ConvID}).

% Called when conversation setup succeeds
conversation_success(MonitorPID, ProtocolName, RoleName, ConvID, RoutingTable) ->
  gen_server2:cast(MonitorPID, {ssa_session_established, ProtocolName, RoleName, ConvID, RoutingTable}).

% Called when conversation setup failed, for whatever reason
conversation_setup_failed(MonitorPID, ProtocolName, RoleName, Error) ->
  gen_server2:cast(MonitorPID, {ssa_conversation_setup_failed, ProtocolName, RoleName, Error}).

become(MonitorPID, RegAtom, RoleName, Operation, Arguments) ->
  gen_server2:call(MonitorPID, {become, RoleName, RegAtom, Operation, Arguments}).

invite({ProtocolName, _, ConversationID, MonitorPID}, InviteeMonitorPID, InviteeRoleName) ->
  gen_server2:call(MonitorPID, {send_delayed_invite, ProtocolName,
                               InviteeRoleName, ConversationID,
                               InviteeMonitorPID}).

incoming_invitation(MonitorPID, ProtocolName, RoleName, ConversationID) ->
  gen_server2:call(MonitorPID, {invitation, ProtocolName, RoleName, ConversationID}).

% ConvKey, ProtocolName, Internal invitations, external invitations & endpoints
% Internal: [Role]
% External: [{Role, Endpoint}]
% Returns: {ok, SubsessionID} | {error, Err}
start_subsession(ConvKey, ProtocolName,InternalInvitations, ExternalInvitations) ->
  {_, _, _, MonitorPID} = ConvKey,
  gen_server2:cast(MonitorPID, {start_subsession, ConvKey, ProtocolName,
                                InternalInvitations, ExternalInvitations}).

send_message({ProtocolName, RoleName, ConversationID, MonitorPID},
             Recipients, MessageName, Types, Payload) ->
  gen_server2:call(MonitorPID,
                  {send_msg, ProtocolName, RoleName, ConversationID,
                   Recipients, MessageName, Types, Payload}).

send_message_noerr({ProtocolName, RoleName, ConversationID, MonitorPID},
             Recipients, MessageName, Types, Payload) ->
  gen_server2:cast(MonitorPID,
                  {send_msg_noerr, ProtocolName, RoleName, ConversationID,
                   Recipients, MessageName, Types, Payload}).

direct_deliver_message(MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:call(MonitorPID, {direct_deliver_msg, ProtocolName, RoleName, ConvID, Msg}).

direct_deliver_message_noerr(MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:cast(MonitorPID, {direct_deliver_msg_noerr, ProtocolName, RoleName, ConvID, Msg}).

queue_message(MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:call(MonitorPID, {queue_msg, ProtocolName, RoleName, ConvID, Msg}).

commit_message(MonitorPID, MsgRef) ->
  gen_server2:cast(MonitorPID, {commit_msg, MsgRef}).

drop_message(MonitorPID, MsgRef) ->
  gen_server2:cast(MonitorPID, {drop_msg, MsgRef}).

% Called by the conversation instance to notify the actor that the conversation has died
conversation_ended(MonitorPID, CID, Reason) ->
  gen_server2:cast(MonitorPID, {conversation_ended, CID, Reason}).

make_call(MonitorPID, ProtocolName, RoleName, ConvID, Recipient,
          MessageName, Payload) ->
  gen_server2:call(MonitorPID, {make_call, ProtocolName, RoleName, ConvID,
                                Recipient, MessageName, Payload}).

incoming_call_request(MonitorPID, ProtocolName, RoleName, ConvID,
                      Message, From) ->
  gen_server2:call(MonitorPID, {incoming_call_req, ProtocolName, RoleName, ConvID,
                                Message, From}).

outgoing_call_response(MonitorPID, ProtocolName, RoleName, ConvID, Recipient, MessageName,
                       Payload, From) ->
  gen_server2:call(MonitorPID, {outgoing_call_resp, ProtocolName, RoleName, ConvID,
                                Recipient, MessageName, Payload, From}).

incoming_call_response(MonitorPID, ProtocolName, RoleName, ConvID, Message, From) ->
  gen_server2:cast(MonitorPID, {incoming_call_resp, ProtocolName, RoleName, ConvID,
                                Message, From}).

subsession_setup_failed(MonitorPID, SubsessionName, SubsessionPID, InitiatorPN,
                        InitiatorRN, InitiatorCID, Reason) ->
  gen_server2:cast(MonitorPID, {subsession_setup_failed, SubsessionName, SubsessionPID,
                                InitiatorPN, InitiatorRN, InitiatorCID, Reason}).

subsession_terminated(MonitorPID, ParentConvID, InitiatorRole, SubsessionID) ->
  gen_server2:cast(MonitorPID, {subsession_terminated, ParentConvID, InitiatorRole,
                                SubsessionID}).

subsession_success(MonitorPID, SubsessionName, InitiatorPN, InitiatorRN, InitiatorCID,
                   Result) ->
  gen_server2:cast(MonitorPID, {subsession_success, SubsessionName, InitiatorPN, InitiatorRN,
                                InitiatorCID, Result}).

subsession_failure(MonitorPID, SubsessionName, InitiatorPN, InitiatorRN, InitiatorCID,
                   Reason) ->
  gen_server2:cast(MonitorPID, {subsession_failure, SubsessionName, InitiatorPN, InitiatorRN,
                                InitiatorCID, Reason}).

start_link(ActorTypeName, Args, Options) ->
  gen_server2:start_link(actor_monitor, [{ActorTypeName, Args}], Options).

start_link(RegName, ActorTypeName, Args, Options) ->
  gen_server2:start_link(RegName, actor_monitor, [{ActorTypeName, Args}], Options).

is_actor_alive(MonitorPID) ->
  try gen_server2:call(MonitorPID, ping) of
    pong -> true
  catch
    _:_Err ->
      false
  end.

