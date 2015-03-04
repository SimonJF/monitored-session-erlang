-module(actor_monitor).

-behaviour(gen_server).
-compile(export_all).

-record(conv_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the attached actor type
                     active_protocols, % Protocols which we're currently involved in
                     protocol_role_map, % Roles for each protocol
                     monitors}). % Monitors for each role we play in each protocol

% Monitor State
% Actor PID (as before)
% Actor type name (as before)
% Protocol <-|-> Conversation ID mapping (as we can be involved in mult protocols)
% Protocol |-> Role mapping (where Role is the role we can play in a protocol)
% Monitors (Protocol |-> Monitor Instance)
% Active protocol
%
% New workflow:
% Always handle conversation invitation messages.
%  - Accept if we're not involved in a protocol of that type.
%  - Reject if we're not registered to partake in that protocol.
%
% Always handle conversation termination messages, removing from active
% conversation list.
%
% What to do when we get a message for a role that's not currently active?
% In the paper, it's scheduled co-operatively, meaning only one can be active at
% once. I don't know why though. As a temp thing, we could just ignore them, and
% mark it as a fixme, at least until we get the very basic scaffolding going.
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
              active_protocols=bidirectional_map:new(), % Protocols which we're currently involved in
              protocol_role_map=ProtocolRoleMap, % Roles for each protocol
              monitors=orddict:new()}. % Monitors for each role we play in each protocol

load_monitors([], MonitorDict, _) ->
  MonitorDict;
load_monitors([{ProtocolName, RoleName}|XS], MonitorDict, State) ->
  MonitorRes = protocol_registry:get_monitor(ProtocolName, RoleName),
  case MonitorRes of
    {ok, {ok, Monitor}} -> NewDict = orddict:store(ProtocolName, Monitor, MonitorDict),
                      load_monitors(XS, NewDict, State);
    Err ->
      monitor_warn("Could not load monitor for protocol ~s, role ~s: ~p ",
                   [ProtocolName, RoleName, Err], State),
      load_monitors(XS, MonitorDict, State)
  end.

% Initialises the basic monitor state with some default values.
init([ActorPid, ActorTypeName, ProtocolRoleMap]) ->
  % Firstly, create a fresh state with all of the information we've been given
  BidirectionalPRM = bidirectional_map:from_orddict(ProtocolRoleMap),
  State = fresh_state(ActorPid, ActorTypeName, BidirectionalPRM),
  % Next, we load the monitors.
  MonitorDict = load_monitors(orddict:to_list(ProtocolRoleMap),
                              orddict:new(),
                              State),
  {ok, State#conv_state{monitors=MonitorDict}}.



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
  RoleFindRes = bidirectional_map:find_left(ProtocolName, ProtocolRoleMap),
  AlreadyFulfilled = bidirectional_map:contains_left(ProtocolName, ActiveProtocols),
  case {RoleFindRes, AlreadyFulfilled} of
    {_, true} -> {error, already_fulfilled};
    {{ok, RoleRes}, false} when RoleRes == RoleName ->
      % We can fulfil it!
      NewActiveProtocols = bidirectional_map:store(ProtocolName, ConversationID, ActiveProtocols),
      % TODO: Try-Catch round this, in case the conversation goes away
      Res = gen_server:call(ConversationID, {accept_invitation, RoleName}),
      case Res of
        % Also set the role as active.
        ok -> {ok, State#conv_state{active_protocols=NewActiveProtocols}};
        {error, Err} -> {error, Err}
      end;
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
  NewActiveProtocols = bidirectional_map:remove_right(ConversationID, ActiveProtocols),
  NewState = State#conv_state{active_protocols=NewActiveProtocols},
  monitor_info("Exiting conversation ~p.~n", [ConversationID], State),
  {reply, ok, NewState}.


% Here, we deliver the message to the attached actor (which is a gen_server).
deliver_incoming_message(Protocol, Role, Msg, State) ->
  RecipientPID = State#conv_state.actor_pid,
  gen_server:cast(RecipientPID, {Protocol, Role, Msg}).

deliver_outgoing_message(Msg, ConversationID) ->
  gen_server:cast(ConversationID, {outgoing_msg, Msg}).

% Handles an incoming message. Checks whether we're in the correct conversation,
% then grabs the monitor, then checks / updates the monitor state.
handle_incoming_message(MessageData, ConversationID, State) ->
  monitor_info("Handling incoming message ~p", [MessageData], State),
  Res = monitor_msg(recv, MessageData, ConversationID, State),
  case Res of
    {ok, NewState} -> {noreply, NewState};
    _Err -> {noreply, State} % assuming the error has been logged already
  end.

handle_become(RoleName, Operation, Arguments, State) ->
  RecipientPID = State#conv_state.actor_pid,
  ProtocolRoleMap = State#conv_state.protocol_role_map,
  ProtocolRes = bidirectional_map:find_right(RoleName, ProtocolRoleMap),
  case ProtocolRes of
    {ok, ProtocolName} ->
      gen_server:cast(RecipientPID, {ProtocolName, RoleName, {become, Operation, Arguments}}),
      {reply, ok, State};
    error -> {reply, error, bad_role}
  end.


handle_outgoing_message(CurrentProtocol, _CurrentRole, Recipients,
                        MessageName, Types, Payload, State) ->
  % Firstly, we need to get the conversation ID, based on the current role
  ProtocolRoleMap = State#conv_state.protocol_role_map,
  ActiveProtocols = State#conv_state.active_protocols,
  RoleRes = bidirectional_map:find_left(CurrentProtocol, ProtocolRoleMap),
  ConversationIDRes = bidirectional_map:find_left(CurrentProtocol, ActiveProtocols),
  case {ConversationIDRes, RoleRes} of
    {{ok, ConversationID}, {ok, RoleName}} ->
      MessageData = message:message(make_ref(), RoleName, Recipients,
                                    MessageName, Types, Payload),
      MonitorRes = monitor_msg(send, MessageData, ConversationID, State),
      case MonitorRes of
        {ok, NewState} -> {reply, ok, NewState};
        Err -> {reply, Err, State}
      end;
    {Err, ok} ->
      monitor_warn("Couldn't find conversation for active protocol ~s.~n",
                   [CurrentProtocol], State),
      {reply, Err, State};
    {_, Err} ->
      monitor_warn("Couldn't find current role for active protocol ~s.~n",
                   [CurrentProtocol], State),
      {reply, Err, State}
  end.


monitor_msg(CommType, MessageData, ConversationID, State) ->
  Monitors = State#conv_state.monitors,
  % Get the protocol name for the conversation
  ActiveProtocols = State#conv_state.active_protocols,
  ProtocolRoleMap = State#conv_state.protocol_role_map,
  ConversationProtocolResult = bidirectional_map:find_right(ConversationID, ActiveProtocols),
  MonitorFunction = case CommType of
                      send -> fun monitor:send/2;
                      recv -> fun monitor:recv/2
                    end,
  case ConversationProtocolResult of
    {ok, ProtocolName} ->
      % Set the current protocol, so that it can be used when sending
      % messages later on.
      CurrentRole = bidirectional_map:fetch_left(ProtocolName, ProtocolRoleMap),
      % Find the monitor instance for the current role
      MonitorInstance = orddict:find(ProtocolName, Monitors),
      case MonitorInstance of
        {ok, Monitor} ->
          MonitorResult = MonitorFunction(MessageData, Monitor),
          case MonitorResult of
            {ok, NewMonitorInstance} ->
              NewMonitors = orddict:store(ProtocolName,
                                          NewMonitorInstance,
                                          Monitors),
              NewState = State#conv_state{monitors=NewMonitors},
              % Do delegation stuff here
              % If we're sending, then pop it to the conversation instance process
              % If we're receiving, then delegate to user session actor code.
              case CommType of
                send -> deliver_outgoing_message(MessageData, ConversationID);
                recv -> deliver_incoming_message(ProtocolName, CurrentRole, MessageData, NewState)
              end,
              {ok, NewState};
            {error, Err, _Monitor} ->
              monitor_warn("Monitor failed when processing message ~p (~p). Error: ~p~n",
                           [MessageData, CommType, Err], State),
              {error, Err}
          end;
        error ->
          monitor_warn("Could not find monitor for role ~s. Monitors: ~p.~n", [CurrentRole,
                                                                              orddict:to_list(Monitors)], State),
          {error, bad_monitor}
      end;
    error ->
      monitor_error("Could not find protocol for conversation ID ~p.",
                    [ConversationID], State),
      {error, bad_protocol}
  end.


handle_send_delayed_invite(ProtocolName, InviteeMonitorPid, RoleName, State) ->
  ActiveProtocols = State#conv_state.active_protocols,
  ConversationIDRes = bidirectional_map:find_left(ProtocolName, ActiveProtocols),
  case ConversationIDRes of
    {ok, ConversationID} ->
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
        {ok, Err} -> {reply, err, State};
        % Couldn't find protocol
        Err -> {reply, Err, State}
      end;
    error -> {error, not_in_conversation}
  end.

% Synchronous messages:
%  * Invitation
%  * Termination
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _Sender, State) ->
  handle_invitation(ProtocolName, RoleName, ConversationID, State);
handle_call({terminate_conversation, ConversationID}, _Sender, State) ->
  handle_terminate_conversation(ConversationID, State);
handle_call({send_msg, CurrentProtocol, CurrentRole, Recipients,
             MessageName, Types, Payload}, _Sender, State) ->
  handle_outgoing_message(CurrentProtocol, CurrentRole, Recipients,
                          MessageName, Types, Payload, State);
handle_call({become, RoleName, Op, Arguments}, _Sender, State) ->
  handle_become(RoleName, Op, Arguments, State);
handle_call({send_delayed_invite, ProtocolName, InviteeMonitorPid, RoleName},
            _Sender, State) ->
  handle_send_delayed_invite(ProtocolName, InviteeMonitorPid, RoleName, State);
handle_call(Other, Sender, State) ->
  monitor_warn("Received unhandled synchronous message ~p from PID ~p.",
               [Other, Sender], State),
  {reply, unhandled, State}.

% Module:handle_cast(Request, State) -> Result
% Only async messages are actually data ones.
% Delivering these, we'll need a conv ID, I think.
handle_cast({message, ConversationID, MessageData}, State) ->
  handle_incoming_message(MessageData, ConversationID, State);
handle_cast(Other, State) ->
  monitor_warn("Received unhandøled async message ~p.", [Other], State),
  {noreply, State}.

handle_info(Info, State) ->
  monitor_warn("Received unhandled info message ~p.", [Info], State),
  {noreply, State}.

terminate(Reason, State) ->
  monitor_error("Terminated for reason ~p.", [Reason], State),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.

