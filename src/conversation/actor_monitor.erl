-module(actor_monitor).

-behaviour(gen_fsm).
-compile(export_all).
-record(conv_state, {actor_pid, % PID of the attached actor
                     actor_type_name, % Name of the actor type
                     conversation_id, % Conversation ID
                     available_roles, % Roles the actor has the ability to play
                     conversation_roles, % Roles the actor is playing in the conversation
                     monitors, % Set of monitors
                     active_role}). % Currently-active role

% Note that the state of the actual actor (ie, what's passed to it whenever
% a message is received / operation is called, etc) is contained in the
% actual actor itself.

% Actor monitor. Contains the per-conversation state of the session actor.
% All messages go through this process, prior to being forwarded to the
% session actor itself.

% Session actor workflow. This is all system-y stuff, so is handled by the
% monitor process.
%
% Workflow:
%  * Listen for invitation messages.
%    * Actors can be invited for multiple roles. This means we've got
%      to treat this a bit like an FSM:
%        Idle -> Setup -> Working -> Idle, where...
%          - Idle is when the actor is not active in any conversation;
%          - Setup is entered when the first invitation message is received,
%            and waits for either further invitation messages *from the same CID*
%            or for a system message indicating that the conversation setup is
%            complete -- this will be sent by the protocol actor when all roles
%            have been fulfilled. This transitions us into the working state...
%          - Working is the state in which the Actual Communication occurs...
%          - And finally, once the conversation is over, we clear the
%            conversation-specific state and go back to the idle state.
%            How do we know it's over? When both monitors are in final states?
%            Do we wait on a message from the protocol process?
%
%    Idle State
%    ==========
%    When an invitation message is received and we're in the idle state, then
%    we need to do the Conversation Setup. This is...
%
%      * Create conversation state with conversation ID contained in invite msg
%      * Do role setup
%
%      Role Setup
%      ----------
%        * Add the role to which we've been invited into the CurrentRoles list
%        * Retrieve the monitor for the role from the protocol / actor type process
%        * Add the new monitor into the Role |-> Monitor mapping
%
%    Setup State
%    ===========
%    In this state, we await more role invitation messages.
%      * If we get a role invitation message for a conversation we're already in,
%        for a role we've not fulfilled, then we'll need to do the role setup for
%        that particular role.
%      * If we've fulfilled that role already, {error, role_already_fulfilled}
%         - This shouldn't really happen; it's more an internal error
%      * If it's for a different conversation, {error, already_occupied}
%         - If this happens, then that's fine -- it just means that the protocol
%           actor should delegate this invitation to a different actor instance instead.
%
%    Once the protocol instance has populated all roles with actor instances, then
%    we'll be needing to transition into the Working state.
%
%    Working State
%    =============
%    The working state is the main body of the conversation logic. This means that
%    we'll be carrying out the main communication here.
%    If we're the conversation initiator, the protocol process will send us a start_protocol
%    system message, which will set the active role and call the first_message callback.
%
%    We first check to see whether the message that has been received is a system message.
%    If so, then we'll need to handle that and go back to the start.
%
%    If not then we'll check the incoming message against the monitor.




%% OTP Callbacks for gen_fsm.

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
  {next_state, working, StateData}.


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
