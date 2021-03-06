-module(actor_monitor).

-behaviour(gen_server2).
-compile(export_all).
-import(util, [list_contains/2]).
-record(monitor_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the attached actor type
                     active_protocols, % [{ProtocolName, RoleName, ConversationID}]
                     registered_become_conversations, % Atom |-> Conversation
                     monitors, % (RoleName, ConversationID) |-> Monitor
                     routing_table, % ConversationID |-> [(Role, Monitor PID)]
                     queued_messages, % Message ID |-> Message
                     pending_calls, % Call ID |-> Monitor, post-send
                     pending_subsessions, % Subsession ID |-> Monitor, pre-start
                     participant_monitor_refs % {Conv ID, Role} |-> Monitor reference
                                              % Used for push-based detection.
                    }).

% New monitor state
% Say we can be involved in multiple protocols, and multiple roles within each
% protocol.
% In order to do this, we need each message to carry the conversation ID.
% active_protocols then becomes a map from protocol name |-> roles we're currently in
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

fresh_state(ActorTypeName) ->
  #monitor_state{
              actor_type_name=ActorTypeName, % Name of the attached actor type
              active_protocols=orddict:new(),
              registered_become_conversations=orddict:new(),
              monitors=orddict:new(),
              routing_table=orddict:new(),
              queued_messages=orddict:new(),
              pending_calls=orddict:new(),
              pending_subsessions=orddict:new(),
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
  % Firstly, create a fresh state with all of the information we've been given
  % Next, we load the monitors.
  State = fresh_state(ActorTypeName),
  {ok, ActorPID} = ssa_gen_server:start_actor_process(ActorTypeName, Args, self()),
  {ok, State#monitor_state{actor_pid=ActorPID}}.


% Called when we've been invited to fufil a role.
% If we can fulfil the role and haven't already fulfilled the role, then
% load the empty FSM into the monitors list.
% Returns {ok, NewState} if we can fulfil the role, or either:
%   * {error, already_fulfilled} --> if we've already fulfilled the role
%   * {error, cannot_fulfil} --> if this role isn't offered by the actor
add_role(ProtocolName, RoleName, ConversationID, Monitor, State) ->
  ActiveProtocols = State#monitor_state.active_protocols,
  Monitors = State#monitor_state.monitors,
  ActorPID = State#monitor_state.actor_pid,
  % Check to see whether we already have an entry for the protocol-role-cid
  % triple. If not, then we can fulfil it.
  AlreadyFulfilled =
  lists:any(fun(ActiveTuple) ->
                ActiveTuple == {ProtocolName, {RoleName, ConversationID}} end,
            orddict:to_list(ActiveProtocols)),
  if AlreadyFulfilled -> {error, already_fulfilled};
     not AlreadyFulfilled ->
       JoinRequestResult =
         ssa_gen_server:join_conversation_request(ActorPID, ProtocolName,
                                                  RoleName, ConversationID),
       case JoinRequestResult of
         accept ->
           NewActiveProtocols = orddict:append(ProtocolName,
                                               {RoleName, ConversationID}, ActiveProtocols),
           NewMonitors = orddict:store({ConversationID, RoleName},
                                       Monitor, Monitors),
           {ok, State#monitor_state{active_protocols=NewActiveProtocols,
                                    monitors = NewMonitors}};
         decline ->
           {error, actor_declined}
       end
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
handle_invitation(ProtocolName, RoleName, ConversationID, Monitor, State) ->
  AddRoleResult = add_role(ProtocolName, RoleName, ConversationID, Monitor, State),
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
deliver_messages(Roles, RoleMap, P, R, C, Msg, State) ->
  deliver_messages_inner(Roles, RoleMap, P, R, C, Msg, {[], [], [], []}, State).
deliver_messages_inner([], _RoleMap, _, _, _, _, Res, State) -> {Res, State};
deliver_messages_inner([Role|Roles], RoleMap, Protocol, RoleName, ConvID, Msg_NoID, {Ok, D, R, NF}, State) ->
  MessageID = make_ref(),
  Msg = message:add_message_id(Msg_NoID, MessageID),
  case orddict:find(Role, RoleMap) of
    {ok, Endpoint} ->
      if (Endpoint == self()) ->
           io:format("Performing reflexive send in ~p of msg ~p~n", [Role, Msg]),
           case monitor_and_queue(Protocol, Role, ConvID, Msg, State) of
             {ok, NewState} ->
               deliver_messages_inner(Roles, RoleMap, Protocol, RoleName,
                                      ConvID, Msg, {[{Endpoint, MessageID}|Ok],
                                                    D, R, NF}, NewState);
             {{error, _Err}, _} ->
               deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                                     Msg, {Ok, D, [Role|R], NF}, State)
           end;
         (Endpoint =/= self()) ->
            try actor_monitor:queue_message(Endpoint, Protocol, Role, ConvID, Msg) of
              ok ->
                % Delivered successfully, add to Ok list
                deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                                  Msg, {[{Endpoint, MessageID}|Ok], D, R, NF}, State);
              {error, _Err} ->
                % Node alive, but monitor rejected message, add to rejected list
                deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                                  Msg, {Ok, D, [Role|R], NF}, State)
            catch
              _:Err ->
                %io:format("Error in queue message call: ~p~n", [Err]),
                % Actor offline, add to Dead list
                deliver_messages_inner(Roles, RoleMap, Protocol, RoleName, ConvID,
                                  Msg, {Ok, [Role|D], R, NF}, State)
            end
      end;
    error ->
      % Role not found -- user entered it incorrectly. Add to not found
      deliver_messages_inner(Role, RoleMap, Protocol, RoleName, ConvID,
                        Msg, {Ok, D, R, [Role|NF]}, State)
  end.

% Commit a message to a set of endpoints
commit_outgoing_messages([]) -> ok;
commit_outgoing_messages([{EP, MessageID}|EPs]) ->
  actor_monitor:commit_message(EP, MessageID),
  commit_outgoing_messages(EPs).

% Drop a message from a set of endpoints
drop_outgoing_messages([]) -> ok;
drop_outgoing_messages([{EP, MessageID}|EPs]) ->
  actor_monitor:drop_message(EP, MessageID),
  drop_outgoing_messages(EPs).

%monitor_and_deliver(ProtocolName, RoleName, ConvID, Msg, State) ->
direct_deliver_outgoing_message(Recipient, ConvRoutingTable, ProtocolName,
                                ConvID, Msg, State) ->
  case orddict:find(Recipient, ConvRoutingTable) of
    {ok, Endpoint} ->
      if (Endpoint == self()) ->
        monitor_and_deliver(ProtocolName, Recipient, ConvID, Msg, State);
      (Endpoint =/= self()) ->
        Res =
          actor_monitor:direct_deliver_message(Endpoint, ProtocolName,
                                               Recipient, ConvID, Msg),
         {Res, State}
      end;
    error -> {error, {nonexistent_role, Recipient}}
  end.

% Lookup the destination role, and forward to its monitor.
deliver_outgoing_message(ProtocolName, RoleName, ConvID, Msg, State) ->
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
                                       ConvID, Msg, State);
     RecipientLength =/= 1 ->
       MonitorAndQueueRes = deliver_messages(Recipients, ConvRoutingTable,
                                             ProtocolName, RoleName, ConvID, Msg, State),
       case MonitorAndQueueRes of
         {{OkEndpoints, [], [], []}, NewState} ->
           % Everything was successful
           commit_outgoing_messages(OkEndpoints),
           {ok, NewState};
         {{OkEndpoints, D, R, NF}, NewState} ->
           drop_outgoing_messages(OkEndpoints),
           {{error, {queue_failed, D, R, NF}}, NewState}
       end
  end.


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
                        MessageName, Types, Payload, State) ->
  % Construct a message instance, send to the monitor, and check the result
  MessageData = message:message(make_ref(), RoleName, Recipients,
                                MessageName, Types, Payload),

  MonitorRes = monitor_msg(send, MessageData, ProtocolName, RoleName,
                          ConversationID, State),
  case MonitorRes of
      {ok, NewMonitorInstance} ->
        Monitors = State#monitor_state.monitors,
        NewMonitors = orddict:store({ConversationID, RoleName},
                                    NewMonitorInstance,
                                    Monitors),
        NewState = State#monitor_state{monitors=NewMonitors},
        {OutgoingRes, NewState1}  =
          deliver_outgoing_message(ProtocolName, RoleName, ConversationID,
                                   MessageData, NewState),
        % Update monitors only if sending was successful
        NewState2 =
          if OutgoingRes == ok -> NewState1;
             OutgoingRes =/= ok -> State
          end,

        if OutgoingRes =/= ok ->
             handle_send_failed(ConversationID, RoleName, State);
           OutgoingRes == ok -> ok
        end,
        {OutgoingRes, NewState2};
      Err ->
        {Err, State}
  end.

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
              error_logger:info_msg("Successfully sent call request~n"),
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
  error_logger:info_msg("Handling outgoing call response pines~n"),
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
  error_logger:info_msg("Handling incoming call response~n"),
  Response = message:message_payload(Message),
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
                     %routing_table, % ConversationID |-> [(Role, Monitor PID)]
  MonitorPID = self(),
  % Check against the initiator's monitor.
  Monitors = State#monitor_state.monitors,
  ParentRoutingTable = orddict:fetch(InitiatorCID, State#monitor_state.routing_table),
  MonitorInstance = orddict:fetch({InitiatorCID, InitiatorRN}, Monitors),
  MonitorRes = monitor:start_subsession(SubsessionName, InternalInvitations,
                                        ExternalInvitations, MonitorInstance),
  PendingSubsessions = State#monitor_state.pending_subsessions,
  case MonitorRes of
    {ok, NewMonitorInstance} ->
       % Start the subsession
       SubsessionProcRes = conversation_instance_sup:start_subsession_instance(SubsessionName,
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
                                                              ExternalInvitations, ParentRoutingTable),
           {noreply, NewState};
         % Couldn't start the subsesion process
         error ->
           subsession_setup_failed(MonitorPID, SubsessionName, undefined,
                                   InitiatorPN, InitiatorRN, InitiatorCID,
                                   bad_subsesion_proc),
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
handle_call({invitation, ProtocolName, RoleName, ConversationID, Monitor}, _Sender, State) ->
  handle_invitation(ProtocolName, RoleName, ConversationID, Monitor, State);
handle_call({send_msg, CurrentProtocol, CurrentRole, ConversationID, Recipients,
             MessageName, Types, Payload}, _Sender, State) ->
  {Reply, NewState} =
    handle_outgoing_message(CurrentProtocol, CurrentRole, ConversationID, Recipients,
                            MessageName, Types, Payload, State),
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

incoming_invitation(MonitorPID, ProtocolName, RoleName, ConversationID, Monitor) ->
  gen_server2:call(MonitorPID, {invitation, ProtocolName, RoleName, ConversationID, Monitor}).

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

direct_deliver_message(MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:call(MonitorPID, {direct_deliver_msg, ProtocolName, RoleName, ConvID, Msg}).

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
  gen_server2:call(MonitorPID, {incoming_call_resp, ProtocolName, RoleName, ConvID,
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

