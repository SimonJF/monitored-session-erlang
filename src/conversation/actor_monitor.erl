-module(actor_monitor).

-behaviour(gen_server).
-compile(export_all).
-import(util, [list_contains/2]).
-record(conv_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the attached actor type
                     active_protocols, % [{ProtocolName, RoleName, ConversationID}]
                     protocol_role_map, % Protocol Name |-> [Role]
                     registered_become_conversations, % Atom |-> Conversation
                     clean_monitors, % Clean monitors: (ProtocolName, RoleName) |-> Monitor
                     monitors % (RoleName, ConversationID) |-> Monitor
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
% New architecture for actor_monitor: use a gen_server instead of a gen_fsm, as
% we no longer have to differentiate between the idle / setup / working states.
% As soon as we respond to the invitation message, we can partake in interactions
% for the registered conversation ID.

log_msg(Func, Format, Args, State) ->
  InfoStr = "Actor ~p, actor PID ~p, monitor instance ~p.",
  InfoArgs = [State#conv_state.actor_type_name,
              State#conv_state.actor_pid,
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

fresh_state(ActorPid, ActorTypeName, ProtocolRoleMap) ->
  #conv_state{actor_pid=ActorPid,
              actor_type_name=ActorTypeName, % Name of the attached actor type
              active_protocols=orddict:new(),
              protocol_role_map=ProtocolRoleMap, % Roles for each protocol
              registered_become_conversations=orddict:new(),
              monitors=orddict:new(),
              clean_monitors=orddict:new()
             }.

load_monitors([], MonitorDict, _) ->
  MonitorDict;
load_monitors([{ProtocolName, RoleNames}|XS], MonitorDict, State) ->
  NewMonitorDict = load_protocol_monitors(ProtocolName, RoleNames,
                                          MonitorDict, State),
  load_monitors(XS, NewMonitorDict, State).

load_protocol_monitors(_ProtocolName, [], MonitorDict, _State) -> MonitorDict;
load_protocol_monitors(ProtocolName, [Role|Roles], MonitorDict, State) ->
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



% Initialises the basic monitor state with some default values.
init([ActorPid, ActorTypeName, ProtocolRoleMap]) ->
  % Firstly, create a fresh state with all of the information we've been given
  State = fresh_state(ActorPid, ActorTypeName, ProtocolRoleMap),
  % Next, we load the monitors.
  MonitorDict = load_monitors(orddict:to_list(ProtocolRoleMap),
                              orddict:new(),
                              State),
  {ok, State#conv_state{clean_monitors=MonitorDict}}.



% Called when we've been invited to fufil a role.
% If we can fulfil the role and haven't already fulfilled the role, then
% load the empty FSM into the monitors list.
% Returns {ok, NewState} if we can fulfil the role, or either:
%   * {error, already_fulfilled} --> if we've already fulfilled the role
%   * {error, cannot_fulfil} --> if this role isn't offered by the actor
add_role(ProtocolName, RoleName, ConversationID, State) ->
  % Firstly, check to see we're registered for the role
  ProtocolRoleMap = State#conv_state.protocol_role_map,
  ActiveProtocols = State#conv_state.active_protocols,
  Monitors = State#conv_state.monitors,
  FreshMonitors = State#conv_state.clean_monitors,
  ActorPID = State#conv_state.actor_pid,
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
                  ActiveTuple == {ProtocolName, RoleName, ConversationID} end,
              ActiveProtocols),

    case {RoleFindRes, AlreadyFulfilled, FreshMonitorRes} of
    {true, false, {ok, FreshMonitor}} ->
      % We can theoretically fulfil it. Just need to ask the actor...
      JoinRequestResult = gen_server:call(ActorPID,
                                          {ssa_join_conversation,
                                           ProtocolName,
                                           RoleName,
                                           ConversationID}),
      case JoinRequestResult of
        accept ->
          NewActiveProtocols = orddict:append(ProtocolName, RoleName, ActiveProtocols),
          NewMonitors = orddict:store({ProtocolName, RoleName, ConversationID},
                                      FreshMonitor, Monitors),

          % TODO: Try-Catch round this, in case the conversation goes away
          Res = gen_server:call(ConversationID, {accept_invitation, RoleName}),
          case Res of
            % Now, add the <Protocol, Role, CID> |-> Monitor mapping
            ok -> {ok, State#conv_state{active_protocols=NewActiveProtocols,
                                        monitors=NewMonitors}};
            {error, Err} -> {error, Err}
          end;
        decline ->
          {error, actor_declined}
      end;
    {_, true, _} -> {error, already_fulfilled};
    _Other -> {error, cannot_fulfil}
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

% Handle termination of a conversation -- remove from the conversation
handle_terminate_conversation(ConversationID, State) ->
  ActiveProtocols = State#conv_state.active_protocols,
  NewActiveProtocols = lists:filter(fun({_,_, CID}) -> CID =/= ConversationID end,
                                    ActiveProtocols),
  NewState = State#conv_state{active_protocols=NewActiveProtocols},
  monitor_info("Exiting conversation ~p.~n", [ConversationID], State),
  {reply, ok, NewState}.


% Here, we deliver the message to the attached actor (which is a gen_server).
deliver_incoming_message(Protocol, Role, ConvID, Msg, State) ->
  RecipientPID = State#conv_state.actor_pid,
  gen_server:cast(RecipientPID, {ssa_msg, Protocol, Role, ConvID, Msg}).

deliver_outgoing_message(Msg, ProtocolName, RoleName, ConversationID) ->
  gen_server:call(ConversationID, {outgoing_msg, ProtocolName, RoleName, Msg}).

% Handles an incoming message. Checks whether we're in the correct conversation,
% then grabs the monitor, then checks / updates the monitor state.
handle_incoming_message(MessageData, ProtocolName, RoleName, ConversationID,
                        State) ->
  monitor_info("Handling incoming message ~p", [MessageData], State),
  Res = monitor_msg(recv, MessageData, ProtocolName, RoleName, ConversationID, State),
  case Res of
    {ok, NewState} -> {noreply, NewState};
    _Err -> {noreply, State} % assuming the error has been logged already
  end.

handle_register_conv(ProtocolName, _RoleName, ConversationID, RegAtom, State) ->
  RegisteredConversations = State#conv_state.registered_become_conversations,
  NewRegisteredConversations = orddict:store(RegAtom, {ProtocolName, ConversationID}, RegisteredConversations),
  NewState = State#conv_state{registered_become_conversations = NewRegisteredConversations},
  {reply, ok, NewState}.


handle_become(RegAtom, RoleName, Operation, Arguments, State) ->
  RecipientPID = State#conv_state.actor_pid,
  RegisteredConversations = State#conv_state.registered_become_conversations,
  CIDRes = orddict:find(RegAtom, RegisteredConversations),
  case CIDRes of
    {ok, {ProtocolName, CID}} ->
      gen_server:cast(RecipientPID, {become, ProtocolName, RoleName, Operation, Arguments, CID}),
      {reply, ok, State};
    _ ->
      {reply, error, bad_conversation}
  end.

handle_outgoing_message(CurrentProtocol, CurrentRole, ConversationID, Recipients,
                        MessageName, Types, Payload, State) ->
  % Construct a message instance, send to the monitor, and check the result
  MessageData = message:message(make_ref(), CurrentRole, Recipients,
                                MessageName, Types, Payload),
  MonitorRes = monitor_msg(send, MessageData, CurrentProtocol, CurrentRole,
                           ConversationID, State),
  case MonitorRes of
      {ok, Reply, NewState} -> {Reply, NewState};
      Err ->
        {Err, State}
  end.

monitor_msg(CommType, MessageData, ProtocolName, RoleName, ConversationID, State) ->
  Monitors = State#conv_state.monitors,
  MonitorRes = orddict:find({ProtocolName, RoleName, ConversationID},
                            Monitors),
  MonitorFunction = case CommType of
                      send -> fun monitor:send/2;
                      recv -> fun monitor:recv/2
                    end,
  case MonitorRes of
    {ok, Monitor} ->
      MonitorResult = MonitorFunction(MessageData, Monitor),
      case MonitorResult of
        {ok, NewMonitorInstance} ->
          NewMonitors = orddict:store({ProtocolName, RoleName, ConversationID},
                                      NewMonitorInstance,
                                      Monitors),
          NewState = State#conv_state{monitors=NewMonitors},
          % Do delegation stuff here
          % If we're sending, then pop it to the conversation instance process
          % If we're receiving, then delegate to user session actor code.
          case CommType of
            send ->
              OutgoingRes = deliver_outgoing_message(MessageData, ProtocolName,
                                                     RoleName, ConversationID),
              {ok, OutgoingRes, NewState};
            recv ->
              deliver_incoming_message(ProtocolName, RoleName, ConversationID,
                                       MessageData, NewState),
              {ok, NewState}
          end;
        {error, Err, _Monitor} ->
          monitor_warn("Monitor failed when processing message ~p (~p). Error: ~p~n",
                       [MessageData, CommType, Err], State),
          {error, Err}
      end;
    error ->
      monitor_warn("Could not find monitor for PRC <~s, ~s, ~p>. Monitors: ~p.~n",
                   [ProtocolName, RoleName, ConversationID,
                    orddict:to_list(Monitors)], State),
      {error, bad_monitor}
  end.

handle_send_delayed_invite(ProtocolName, RoleName, ConversationID, InviteeMonitorPid,
                           State) ->
    % TODO maybe, check if in conversation first
    % TODO: Adding the lookup step in actor_monitor as opposed to having
    % ConversationID as part of the ConvKey means we can't invite ourselves
    % to the conversation (deadlock due to synchronous calls).
    % Not a problem at the moment as we can only perform one role per protocol,
    % but might be worth revisiting later on.
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

% Synchronous messages:
%  * Invitation
%  * Termination
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _Sender, State) ->
  handle_invitation(ProtocolName, RoleName, ConversationID, State);
handle_call({terminate_conversation, ConversationID}, _Sender, State) ->
  handle_terminate_conversation(ConversationID, State);
handle_call({send_msg, CurrentProtocol, CurrentRole, ConversationID, Recipients,
             MessageName, Types, Payload}, _Sender, State) ->
  {Reply, NewState} =
    handle_outgoing_message(CurrentProtocol, CurrentRole, ConversationID, Recipients,
                            MessageName, Types, Payload, State),
  {reply, Reply, NewState};
handle_call({become, RoleName, RegAtom, Operation, Arguments}, _Sender, State) ->
  handle_become(RegAtom, RoleName, Operation, Arguments, State);
handle_call({send_delayed_invite, ProtocolName, InviteeRoleName, ConversationID, InviteeMonitorPid},
            _Sender, State) ->
  handle_send_delayed_invite(ProtocolName, InviteeRoleName, ConversationID, InviteeMonitorPid, State);
% Delegate directly to handle_call in monitor
handle_call({register_become, RegAtom, ProtocolName, RoleName, ConvID}, _From, State) ->
  handle_register_conv(ProtocolName, RoleName, ConvID, RegAtom, State);
handle_call(Msg, From, State) ->
  ActorPid = State#conv_state.actor_pid,
  Reply = gen_server:call(ActorPid, {delegate_call, From, Msg}),
  {reply, Reply, State}.

%handle_call(Other, Sender, State) ->
%  monitor_warn("Received unhandled synchronous message ~p from PID ~p.",
%               [Other, Sender], State),
%  {reply, unhandled, State}.

% Module:handle_cast(Request, State) -> Result
% Only async messages are actually data ones.
% Delivering these, we'll need a conv ID, I think.
handle_cast({message, ProtocolName, RoleName, ConversationID, MessageData}, State) ->
  handle_incoming_message(MessageData, ProtocolName, RoleName, ConversationID, State);

handle_cast(Other, State) ->
  ActorPid = State#conv_state.actor_pid,
  gen_server:cast(ActorPid, Other),
  {noreply, State}.

handle_info(Info, State) ->
  %monitor_warn("Received unhandled info message ~p.", [Info], State),
  ActorPid = State#conv_state.actor_pid,
  ActorPid ! Info,
  {noreply, State}.

terminate(Reason, State) ->
  monitor_error("Terminated for reason ~p.", [Reason], State),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.



%% Internal API Functions


deliver_message(MonitorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server:cast(MonitorPID, {message, ProtocolName, RoleName, ConvID, Msg}).

register_become(MonitorPID, RegAtom, ProtocolName, RoleName, ConvID) ->
  gen_server:call(MonitorPID, {register_become, RegAtom, ProtocolName, RoleName, ConvID}).

% Called when conversation setup succeeds
conversation_success(MonitorPID, ProtocolName, RoleName, ConvID) ->
  gen_server:cast(MonitorPID, {ssa_session_established, ProtocolName, RoleName, ConvID}).
%  ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
% Called when conversation setup failed, for whatever reason
conversation_setup_failed(MonitorPID, ProtocolName, RoleName, Error) ->
  gen_server:cast(MonitorPID, {ssa_conversation_setup_failed, ProtocolName, RoleName, Error}).
