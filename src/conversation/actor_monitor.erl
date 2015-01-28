-module(actor_monitor).

-behaviour(gen_server).
-compile(export_all).
% -record(conv_state, {actor_pid, % PID of the attached actor
%                      actor_type_name, % Name of the actor type
%                      conversation_id, % Conversation ID
%                      available_roles, % Roles the actor has the ability to play
%                      conversation_roles, % Roles the actor is playing in the conversation
%                      monitors, % Set of monitors
%                      active_role}). % Currently-active role

-record(conv_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the attached actor type
                     active_protocols, % Protocols which we're currently involved in
                     registered_roles, % Roles for each protocol
                     monitors, % Monitors for each role we play in each protocol
                     current_protocol}). % The currently-active protocol.


% TODO:::::
% I've misunderstood this a bit, and am going to have to make a few changes.
% Basically, we can be involved with multiple protocols, but only one role
% in each.
% This will change the FSM / state a fair bit.
%
% Instead, we should always wait for synchronous invitation messages in
% whichever state we're in, and accept if we're not involved in that
% protocol type already.
%
% Really, we should move from gen_fsm to gen_server.
% Each actor can only play one role per protocol, but can partake in multiple
% protocols. This means that basically what we'll have to do is change
% available_roles to available_protocols, where each entry is a
% {ProtocolName, RoleName} pair.
%
% New state:
% Actor PID (as before)
% Actor type name (as before)
% Protocol |-> Conversation ID mapping (as we can be involved in mult protocols)
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

fresh_state(ActorPid, ActorTypeName, AvailableRoles) ->
  #conv_state{actor_pid=ActorPid,
              actor_type_name=ActorTypeName,
              conversation_id=no_conversation,
              available_roles=[],
              conversation_roles=[],
              monitors=orddict:new(),
              active_role=no_active_role}.

freshen_state(OldState) ->
  ActorPid = OldState#conv_state.actor_pid,
  ActorTypeName = OldState#conv_state.actor_type_name,
  AvailableRoles = OldState#conv_state.available_roles,
  fresh_state(ActorPid, ActorTypeName, AvailableRoles).

% Initialises the basic monitor state with some default values.
init([ActorPid, ActorTypeName, AvailableRoles]) ->
  {ok, fresh_state(ActorPid, ActorTypeName, AvailableRoles)}.


% Called when we've been invited to fufil a role.
% If we can fulfil the role and haven't already fulfilled the role, then
% load the empty FSM into the monitors list (TODO).
% Returns {ok, NewState} if we can fulfil the role, or either:
%   * {error, already_fulfilled} --> if we've already fulfilled the role
%   * {error, cannot_fulfil} --> if this role isn't offered by the actor
add_role(RoleName, State) ->
  AvailableRoles = State#conv_state.available_roles,
  ConversationRoles = State#conv_state.conversation_roles,
  % Check whether we've already fulfilled the role in the conversation
  AlreadyFulfilled = lists:member(RoleName, ConversationRoles),
  if not AlreadyFulfilled ->
      % Check we can fulfil the role
      CanFulfil = lists:member(RoleName, AvailableRoles),
      if CanFulfil ->
           NewConversationRoles = [RoleName|ConversationRoles],
           {ok, State#conv_state{conversation_roles=NewConversationRoles}};
         not CanFulfil ->
           {error, cannot_fulfil}
      end;
    AlreadyFulfilled ->
      {error, already_fulfilled}
  end.




% We'll have a mixture of synchronous and asynchronous messages.
% So...
% System Messages:
%   - Invitation (synchronous)
%   - Ready (synchronous) -- Setup complete, transitions from setup to Working state
%   - Terminate Conversation (asynchronous)
%
% Other messages are all asynchronous.
% If we get invitation messages in anything other than the idle or setup states,
% or if the invitation message is for a different conversation ID, we should send
% back {error, already_occupied}.


% Synchronous callbacks.
% Module:StateName(Event, From, StateData) -> Result
% Result can be...
% {reply,Reply,NextStateName,NewStateData}
%  | {reply,Reply,NextStateName,NewStateData,Timeout}
%  | {reply,Reply,NextStateName,NewStateData,hibernate}
%  | {next_state,NextStateName,NewStateData}
%  | {next_state,NextStateName,NewStateData,Timeout}
%  | {next_state,NextStateName,NewStateData,hibernate}

handle_invitation(ConversationID, RoleName, CurrentState, StateData) ->
  AddRoleResult = add_role(RoleName, StateData),
  % Try and add the role.
  % If we succeed, add the role to the conversation_roles list, set the
  % conversation ID, and transition to setup.
  % If not (eg we can't fulfil the role), then we stay where we are, and make
  % no changes to the state or state data.
  case AddRoleResult of
    {ok, NewStateData} ->
      error_logger:info_msg("INFO: Actor ~w with PID ~p registered for role
                             ~w.", [NewStateData#conv_state.actor_type_name,
                                    NewStateData#conv_state.actor_pid,
                                    RoleName]),
      {reply, ok, setup, NewStateData};
    {error, Err} ->
      error_logger:warning_msg("WARN: Actor ~w could not fulfil role ~w: ~p",
                               [StateData#conv_state.actor_type_name,
                                RoleName,
                                Err]),
      {reply, {error, Err}, CurrentState, StateData}
  end.

handle_terminate_conversation(ConversationID, CurrentState, StateData) ->
  ActorName = StateData#conv_state.actor_type_name,
  ActorPID = StateData#conv_state.actor_pid,
  CurrentCID = StateData#conv_state.conversation_id,
  if ConversationID == CurrentCID ->
       error_logger:info_msg("INFO: Actor ~w with PID ~p exiting conversation ~p.~n",
                             [ActorName, ActorPID, ConversationID]),
       {next_state, idle, freshen_state(StateData)};
     ConversationID =/= CurrentCID ->
       error_logger:warning_msg("WARN: Terminate conversation (ID ~p) called for monitor in " ++
                                " different conversation (ID ~p) in actor type ~w with PID ~p.~n",
                               [ConversationID, CurrentCID, ActorName, ActorPID]),
       {next_state, CurrentState, StateData}
  end.

% Idle / setup have identical (boring) behaviours for async messages,
% as most setup messages are synchronous.
handle_setup_async(Inv = {invitation, _, _}, CurrentState, StateData) ->
  error_logger:error_msg("ERROR: Monitor got asynchronous invitation message ~p~n", [Inv]),
  {stop, error_invitation_async, StateData};
handle_setup_async({terminate_conversation, ConvID}, CurrentState, StateData) ->
  handle_terminate_conversation(ConvID, CurrentState, StateData);
handle_setup_async(Msg = {message, _MsgData}, CurrentState, StateData) ->
  error_logger:warning_msg("WARN: Got message (~p) in idle state: ignoring.~n ", [Msg, StateData]),
  {next_state, CurrentState, StateData};
handle_setup_async(Other, CurrentState, StateData) ->
  error_logger:warning_msg("WARN: Got unhandled asynchronous message (~p) in ~p state: ignoring.",
                           [Other, CurrentState]),
  {next_state, CurrentState, StateData}.


% Here, we deliver the message to the attached actor (which is a gen_server).
deliver_message(Msg, StateData) ->
  RecipientPID = StateData#conv_state.actor_pid,
  gen_server:cast(RecipientPID, Msg).

handle_incoming_message(Msg = {message, MessageData}, StateData) ->
  Monitors = StateData#conv_state.monitors,
  CurrentRole = StateData#conv_state.active_role,
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
                                   [StateData#conv_state.conversation_id,
                                    StateData#conv_state.actor_type_name,
                                    StateData#conv_state.actor_pid,
                                    CurrentRole, Msg, Err]),
          {next_state, working, StateData}
      end;
    error ->
      error_logger:warning_msg("WARN: Could not find monitor for role ~w in " ++
                               "actor of type ~w (instance PID ~p) in " ++
                               "conversation with ID ~p.~n",
                               [StateData#conv_state.active_role,
                                StateData#conv_state.actor_type_name,
                                StateData#conv_state.actor_pid,
                                StateData#conv_state.conversation_id]),
      {next_state, working, StateData}
  end.


% IDLE (Synchronous): Can handle invitation messages, but nothing else.
% =====================================================================
% =====================================================================
idle({invitation, ConversationID, RoleName}, _Sender, StateData) ->
  NewStateDataData = StateData#conv_state{conversation_id=ConversationID},
  handle_invitation(ConversationID, RoleName, idle, NewStateDataData);
idle(Other, _Sender, StateData) ->
  error_logger:warning_msg("WARN: Monitor got unhandled synchronous messsage ~p in"
                            ++ " idle state. ~n", [Other]),
  {next_state, idle, StateData}.

% SETUP (Synchronous): Can handle invitation messages and ready messages.
% =======================================================================
% =======================================================================
setup({invitation, ConversationID, RoleName}, _Sender, State) ->
  % Check that the conversation ID matches up with the current conversation ID,
  % reject the invitation with already_occupied if not
  CIDMatches = ConversationID == State#conv_state.conversation_id,
  if CIDMatches ->
       handle_invitation(ConversationID, RoleName, setup, State);
     not CIDMatches ->
       {reply, {error, already_occupied}, setup, State}
  end;
setup({ready, ConversationID}, _Sender, State) ->
  % Transition into the working state.
  error_logger:info_msg("Info: Monitor for actor ~p with PID ~p " ++
                        "transitioning into working state. ~n",
                       [State#conv_state.actor_type_name,
                        State#conv_state.actor_pid]),
  {next_state, working, State};
setup(Other, _Sender, State) ->
  error_logger:warning_msg("WARN: Monitor got unhandled synchronous messsage ~p"
                            ++ " in setup state. ~n", [Other]),
  {next_state, idle, State}.

% WORKING (Synchronous): No valid messages
% =======================================================================
% =======================================================================
working(Msg, _Sender, State) ->
  error_logger:warning_msg("WARN: Monitor got synchronous messsage ~p"
                            ++ " in working state. ~n", [Msg]),
  % Transition into the working state.
  {next_state, working, State}.

% handle_terminate_conversation(ConversationID, CurrentState, StateData)

% Asynchronous callbacks.
% Module:StateName(Event, StateData) -> Result
% Result can be...
% {next_state,NextStateName,NewStateData}
%  | {next_state,NextStateName,NewStateData,Timeout}
%  | {next_state,NextStateName,NewStateData,hibernate}
%  | {stop,Reason,NewStateData}




% IDLE (Asynchronous): Can't really do much!
% =====================================================================
% =====================================================================
idle(Msg, StateData) ->
  handle_setup_async(Msg, idle, StateData).

% SETUP (Asynchronous): Can't really do much either!
% =====================================================================
% =====================================================================
setup(Msg, StateData) ->
  handle_setup_async(Msg, setup, StateData).


% WORKING (Asynchronous): Now this is the interesting one!
% In this state, we can send and receive messages -- that is, the protocol
% is in progress.
% In this state, we grab the monitor from the monitor storage, check the incoming
% message against it, and if it corresponds, then pass it to the actor itself.
% It's also possible to terminate the conversation at this point, nuking the
% conversation state and going back to idle.
working(Msg = {message, MessageData}, StateData) ->
  handle_incoming_message(Msg, StateData),
  % Remain in the working state.
  {next_state, working, StateData};
working({terminate_conversation, ConvID}, StateData) ->
  handle_terminate_conversation(ConvID, working, StateData).


% Remaining uninteresting (yet required) OTP callbacks
handle_info(Info, StateName, StateData) ->
  error_logger:warning_msg("WARN: Monitor got info messsage ~p"
                            ++ " in state ~p: ignoring. ~n", [Info, StateName]),
  {next_state, StateName, StateData}.

handle_event(Event, StateName, StateData) ->
  error_logger:warning_msg("WARN: Monitor got synchronous event messsage ~p"
                            ++ " in state ~p: ignoring. ~n", [Event, StateName]),
  {next_state, StateName, StateData}.


handle_sync_event(Event, _From, StateName, StateData) ->
  error_logger:warning_msg("WARN: Monitor got asynchronous event messsage ~p"
                            ++ " in state ~p: ignoring. ~n", [Event, StateName]),
  {next_state, StateName, StateData}.

code_change(OldVersion, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(Reason, _StateName, StateData) ->
  error_logger:error_msg("ERROR: Monitor for actor of type ~w (monitor PID ~p) " ++
                         "terminating because of reason ~p",
                         [StateData#conv_state.actor_type_name, self(), Reason]),

  ok.
