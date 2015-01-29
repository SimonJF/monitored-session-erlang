-module(actor_monitor).

-behaviour(gen_server).
-compile(export_all).

-record(conv_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the attached actor type
                     active_protocols, % Protocols which we're currently involved in
                     protocol_role_map, % Roles for each protocol
                     monitors, % Monitors for each role we play in each protocol
                     current_protocol}). % The currently-active protocol.

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
%
%
% Work for tomorrow!
%  - Implement the above plan for actor_monitor
%  - Figure out what needs to be changed in session_actor, and do those
%  - Get monitor loading / storage / resets going. What stores what?

fresh_state(ActorPid, ActorTypeName, ProtocolRoleMap) ->
  #conv_state{actor_pid=ActorPid,
              actor_type_name=ActorTypeName, % Name of the attached actor type
              active_protocols=bidirectional_map:new(), % Protocols which we're currently involved in
              protocol_role_map=ProtocolRoleMap, % Roles for each protocol
              monitors=orddict:new(), % Monitors for each role we play in each protocol
              current_protocol=undefined}. % The currently-active protocol.

% Initialises the basic monitor state with some default values.
init([ActorPid, ActorTypeName, ProtocolRoleMap]) ->
  {ok, fresh_state(ActorPid, ActorTypeName, ProtocolRoleMap)}.


% Called when we've been invited to fufil a role.
% If we can fulfil the role and haven't already fulfilled the role, then
% load the empty FSM into the monitors list (TODO).
% Returns {ok, NewState} if we can fulfil the role, or either:
%   * {error, already_fulfilled} --> if we've already fulfilled the role
%   * {error, cannot_fulfil} --> if this role isn't offered by the actor
add_role(ProtocolName, RoleName, ConversationID, State) ->
  % Firstly, check to see we're registered for the role
  ProtocolRoleMap = State#conv_state.protocol_role_map,
  ActiveProtocols = State#conv_state.active_protocols,
  RoleFindRes = orddict:find(ProtocolName, ProtocolRoleMap),
  AlreadyFulfilled = bidirectional_map:contains_left(ProtocolName, ActiveProtocols),
  case {RoleFindRes, AlreadyFulfilled} of
    {_, true} -> {error, already_fulfilled};
    {{ok, RoleRes}, false} when RoleRes == RoleName ->
      % We can fulfil it!
      NewActiveProtocols = bidirectional_map:store(ProtocolName, ConversationID),
      {ok, State#conv_state{active_protocols=NewActiveProtocols}};
    _Other -> {error, cannot_fulfil}
  end.


% Handles an invitation to fulfil a role
handle_invitation(ProtocolName, RoleName, ConversationID, StateData) ->
  AddRoleResult = add_role(ProtocolName, RoleName, ConversationID, StateData),
  % Try and add the role.
  % If we succeed, add the role to the conversation_roles list, set the
  % conversation ID, and transition to setup.
  % If not (eg we can't fulfil the role), then we stay where we are, and make
  % no changes to the state or state data.
  case AddRoleResult of
    {ok, NewStateData} ->
      error_logger:info_msg("INFO: Actor ~w with PID ~p registered for role
                             ~w in protocol ~w.~n",
                            [NewStateData#conv_state.actor_type_name,
                             NewStateData#conv_state.actor_pid,
                             RoleName, ProtocolName]),
      {reply, ok, NewStateData};
    {error, Err} ->
      error_logger:warning_msg("WARN: Actor ~w with PID ~p could not fulfil role " ++
                               "~w in protocol ~w; error: ~p.~n",
                               [StateData#conv_state.actor_type_name,
                                StateData#conv_state.actor_pid,
                                RoleName,
                                ProtocolName,
                                Err]),
      {reply, {error, Err}, StateData}
  end.

% Handle termination of a conversation -- remove from the conversation
handle_terminate_conversation(ConversationID, State) ->
  ActorName = State#conv_state.actor_type_name,
  ActorPID = State#conv_state.actor_pid,
  ActiveProtocols = State#conv_state.active_protocols,
  NewActiveProtocols = bidirectional_map:remove_right(ConversationID, ActiveProtocols),
  NewState = State#conv_state{active_protocols=NewActiveProtocols},
  error_logger:info_msg("INFO: Actor ~w with PID ~p exiting conversation ~p.~n",
                       [ActorName, ActorPID, ConversationID]),
  {noreply, NewState}.


% Here, we deliver the message to the attached actor (which is a gen_server).
deliver_message(Msg, StateData) ->
  RecipientPID = StateData#conv_state.actor_pid,
  gen_server:cast(RecipientPID, Msg).

% Handles an incoming message. Checks whether we're in the correct conversation,
% then grabs the monitor, then checks / updates the monitor state.
handle_incoming_message(Msg = {message, MessageData}, ConversationID, StateData) ->
  Monitors = StateData#conv_state.monitors,
  % Get the protocol name for the conversation
  ActiveProtocols = StateData#conv_state.active_protocols,
  ProtocolRoleMap = StateData#conv_state.protocol_role_map,
  CurrentProtocol = StateData#conv_state.current_protocol,
  % TODO: Set CurrentProtocol if it is undefined
  ConversationProtocolResult = bidirectional_map:fetch_right(ConversationID, ActiveProtocols),
  case ConversationProtocolResult of
    {ok, ProtocolName} when CurrentProtocol == ProtocolName ->
      CurrentRole = orddict:fetch(ProtocolName, ProtocolRoleMap),
      % Find the monitor instance for the current role
      % FIXME: This won't work -- we haven't set it yet!
      MonitorInstance = orddict:find(CurrentRole, Monitors),
      case MonitorInstance of
        {ok, Monitor} ->
          MonitorResult = monitor:recv(MessageData, Monitor),
          case MonitorResult of
            {ok, NewMonitorInstance} ->
              NewMonitors = orddict:store(CurrentRole, NewMonitorInstance, Monitors),
              NewStateData = StateData#conv_state{monitors=NewMonitors},
              % Do delegation stuff here
              % Send the MessageData to the session actor, let the logic in there
              % unpack and delegate to user code
              deliver_message(MessageData, NewStateData),
              {next_state, working, NewStateData};
            {error, Err} ->
              error_logger:warning_msg("WARN: Monitor for conversation ID ~p " ++
                                       "in actor ~w (instance PID ~p) with " ++
                                       "active role ~w failed when receiving " ++
                                       "message ~p. Error: ~p.~n",
                                       [ConversationID,
                                        StateData#conv_state.actor_type_name,
                                        StateData#conv_state.actor_pid,
                                        CurrentRole, Msg, Err]),
              {noreply, StateData}
          end;
        error ->
          error_logger:warning_msg("WARN: Could not find monitor for role ~w in " ++
                                   "actor of type ~w (instance PID ~p) in " ++
                                   "conversation with ID ~p.~n",
                                   [CurrentRole,
                                    StateData#conv_state.actor_type_name,
                                    StateData#conv_state.actor_pid,
                                    ConversationID]),
          {noreply, StateData}
      end;
    {ok, ProtocolName} when CurrentProtocol =/= ProtocolName ->
      % Our current protocol doesn't match the protocol associated with the message.
      % Ignore for now, but will have to do something with this later.
      error_logger:warning_msg("WARN: Actor ~w (instance PID ~p) could not " ++
                                   "handle message ~p as it is for a different protocol " ++
                                   "(~p) then the one that is currently running (~p).~n",
                                   [StateData#conv_state.actor_type_name,
                                    StateData#conv_state.actor_pid,
                                    ProtocolName, CurrentProtocol]),
      {noreply, StateData};
    error ->
      error_logger:warning_msg("WARN: Monitor for actor ~w (instance PID ~p) could not " ++
                                   "find protocol for conversation ID ~p.~n",
                                   [StateData#conv_state.actor_type_name,
                                    StateData#conv_state.actor_pid,
                                    ConversationID]),
      {noreply, StateData}
  end;

handle_incoming_message(Other, _CID, StateData) ->
  error_logger:warning_msg("WARN: handle_incoming_message called for non_message ~p " ++
                           "in actor ~p (instance PID ~p)",
                           [Other, StateData#conv_state.actor_type_name,
                            StateData#conv_state.actor_pid]),
  {noreply, StateData}.


% Synchronous messages:
%  * Invitation
%  * Termination
%  handle_invitation(ProtocolName, RoleName, ConversationID, StateData) ->
%  handle_terminate_conversation(ConversationID, CurrentState, State)
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _Sender, State) ->
  handle_invitation(ProtocolName, RoleName, ConversationID, State);
handle_call({terminate_conversation, ConversationID}, _Sender, State) ->
  handle_terminate_conversation(ConversationID, State);
handle_call(Other, Sender, State) ->
  ActorTypeName = State#conv_state.actor_type_name,
  ActorPID = State#conv_state.actor_pid,
  error_logger:warning_msg("WARN: Monitor for actor ~w, instance PID ~p, received " ++
                           "unhandled synchronous messsage ~p from PID ~p.~n",
                           [ActorTypeName, ActorPID, Other, Sender]),
  {noreply, State}.

% Module:handle_cast(Request, State) -> Result
% Only async messages are actually data ones.
% Delivering these, we'll need a conv ID, I think.
handle_cast({msg, ConversationID, MessageData}, State) ->
  handle_incoming_message(MessageData, ConversationID, State);
handle_cast(Other, State) ->
  ActorTypeName = State#conv_state.actor_type_name,
  ActorPID = State#conv_state.actor_pid,
  error_logger:warning_msg("WARN: Monitor for actor ~w, instance PID ~p, received " ++
                           "unhandled asynchronous messsage ~p.~n",
                           [ActorTypeName, ActorPID, Other]),
  {noreply, State}.

handle_info(Info, State) ->
  ActorTypeName = State#conv_state.actor_type_name,
  ActorPID = State#conv_state.actor_pid,
  error_logger:warning_msg("WARN: Monitor for actor ~w, instance PID ~p, received " ++
                           "unhandled info messsage ~p.~n",
                           [ActorTypeName, ActorPID, Info]),
  {noreply, State}.

terminate(Reason, State) ->
  ActorTypeName = State#conv_state.actor_type_name,
  ActorPID = State#conv_state.actor_pid,
  error_logger:warning_msg("ERROR: Monitor for actor ~w, instance PID ~p, terminated " ++
                           "for reason ~p.~n",
                           [ActorTypeName, ActorPID, Reason]),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.

