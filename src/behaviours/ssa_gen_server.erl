-module(ssa_gen_server).
-behaviour(gen_server2).

-compile(export_all).
-record(actor_state, {actor_type_name,
                      monitor_pid,
                      user_state}).

% This is the behaviour for basic session actors.
% The behaviour requires two callbacks:
%  * ssactor_init, which returns the initial user state
%  * ssactor_handle_msg, which handles incoming messages.
%
% The session actor itself has certain bits of internal state, which we
% leverage in order to perform monitoring and message routing. This is:
%  * Conversation process PID: We use this in order to relay messages to
%    the conversation process, which performs Role |-> Endpoint routing.
%
%  * Roles the actor is playing in the current conversation
%
%  * The currently-active role in the actor
%
%  * And finally, a Role |-> Monitor mapping.


% ssactor_init returns some state given some input args
% ssactor_handle_message handles a message
% ssactor_join is called when the actor is invited to participate in a
%   conversation. The user can decide to accept or decline this invitation.
% ssactor_conversation_established is called when all actors have been invited.
% ssactor_conversation_error is called when there was an error establishing the
%   conversation.
behaviour_info(callbacks) ->
    [{ssactor_init,2},
     {ssactor_join,4},
     {ssactor_handle_message, 7},
     {ssactor_become, 5},
     {ssactor_conversation_established, 5},
     {ssactor_conversation_error, 4},
     {ssactor_conversation_ended, 3},
     {ssactor_subsession_complete, 4}, % Name, Result, State, ConvKey
     {ssactor_subsession_failed, 4}, % Name, FailureName, State, ConvKey
     {ssactor_subsession_setup_failed, 4},
     {handle_call, 3},
     {handle_cast, 3},
     {handle_info, 3},
     {terminate, 2}
    ];
behaviour_info(_Other) ->
    undefined.

update_user_state(SystemState, NewUserState) ->
  SystemState#actor_state{user_state = NewUserState}.

log_msg(Func, Format, Args, State) ->
  InfoStr = "SSACTOR: Actor ~p, actor PID ~p, monitor PID ~p.",
  InfoArgs = [State#actor_state.actor_type_name,
              self(),
              State#actor_state.monitor_pid],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

actor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

actor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

actor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).


% gen_server2 callbacks
init([Module, UserArgs, MonitorPID]) ->
  actor_registry:register_actor(Module, MonitorPID),
  UserState = Module:ssactor_init(UserArgs, MonitorPID),
  {ok, #actor_state{actor_type_name=Module,
                    monitor_pid=MonitorPID,
                    user_state=UserState}}.


% Delegate calls, casts (other than ssa internal messages), info messages
% and termination messages to the actor.

delegate_async(Fun, Msg, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  UserResult = case Fun of
                 handle_cast ->
                   Module:handle_cast(Msg, UserState);
                 handle_info ->
                   Module:handle_info(Msg, UserState)
               end,
  % Propagate user state changes without losing system state
  case UserResult of
    {noreply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState};
    {noreply, NewUserState, Arg} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState, Arg};
    {stop, Reason, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {stop, Reason, NewState}
  end.

% Same for synchronous messages, but there are a couple more things we need
% to handle, in particular re: replies
handle_call(ssa_get_monitor_id, _From, State) ->
  {reply, State#actor_state.monitor_pid, State};
handle_call({ssa_join_conversation, ProtocolName, RoleName, ConversationID},
            _From, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  UserResult = Module:ssactor_join(ProtocolName, RoleName, ConversationID, UserState),
  case UserResult of
    {accept, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {reply, accept, NewState};
    {decline, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {reply, decline, NewState}
  end;
handle_call({delegate_call, From, Msg}, _From, State) ->
  % Spoof From value
  handle_call(Msg, From, State);
handle_call(Request, From, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  UserResult = Module:handle_call(Request, From, UserState),
  case UserResult of
    {reply, Reply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {reply, Reply, NewState};
    {reply, Reply, NewUserState, Arg} ->
      NewState = update_user_state(State, NewUserState),
      {reply, Reply, NewState, Arg};
    {noreply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState};
    {noreply, NewUserState, Arg} ->
      NewState = update_user_state(State, NewUserState),
      {noreply, NewState, Arg};
    {stop, Reason, Reply, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {stop, Reason, Reply, NewState};
    {stop, Reason, NewUserState} ->
      NewState = update_user_state(State, NewUserState),
      {stop, Reason, NewState}
  end.

make_conv_key(Protocol, Role, ConvID, MonitorID) ->
  {Protocol, Role, ConvID, MonitorID}.

handle_subsession_setup_failure(SubsessionName, ProtocolName, RoleName,
                                ConvID, Reason, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  MonitorPID = State#actor_state.monitor_pid,
  ConvKey = make_conv_key(ProtocolName, RoleName, ConvID, MonitorPID),
  {ok, NewUserState} =
    Module:ssactor_subsession_setup_failed(SubsessionName, Reason, UserState, ConvKey),
  {noreply, State#actor_state{user_state=NewUserState}}.

handle_subsession_success(SubsessionName, ProtocolName, RoleName, ConvID, SubsessionResult, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  MonitorPID = State#actor_state.monitor_pid,
  ConvKey = make_conv_key(ProtocolName, RoleName, ConvID, MonitorPID),
  {ok, NewUserState} =
    Module:ssactor_subsession_complete(SubsessionName, SubsessionResult, UserState, ConvKey),
  {noreply, State#actor_state{user_state=NewUserState}}.

handle_subsession_failure(SubsessionName, ProtocolName,
                          RoleName, ConvID, FailureName, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  MonitorPID = State#actor_state.monitor_pid,
  ConvKey = make_conv_key(ProtocolName, RoleName, ConvID, MonitorPID),
  {ok, NewUserState} =
    Module:ssactor_subsession_failed(SubsessionName, FailureName, UserState, ConvKey),
  {noreply, State#actor_state{user_state=NewUserState}}.

% Handle incoming user messages. These have been checked by the monitor to
% ensure that they conform to the MPST.
handle_cast({ssa_msg, Protocol, Role, ConversationID, MsgData}, State) ->
                   % {message, _, Sender, _, Op, Types, Payload}}, State) ->
  %actor_info("Processing message ~p", [MsgData], State),
  Sender = message:message_sender(MsgData),
  Op = message:message_name(MsgData),
  Payload = message:message_payload(MsgData),
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  % TODO: ssactor_handle_message currently just returns a new state.
  % Should we have some more complex callback here instead?
  {ok, NewUserState} = Module:ssactor_handle_message(
                        Protocol, Role, ConversationID, Sender, Op, Payload, UserState,
                        {Protocol, Role, ConversationID, State#actor_state.monitor_pid}),
  {noreply, State#actor_state{user_state=NewUserState}};
% Become
handle_cast(_Msg = {become, Protocol, Role, Operation, Arguments, CID}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  {ok, NewUserState} =
    Module:ssactor_become(Protocol, Role, Operation, Arguments,
                          {Protocol, Role, CID, State#actor_state.monitor_pid},
                          UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
% Setup failed
handle_cast(_Msg = {ssa_conversation_setup_failed, Protocol, Role, Err}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  {ok, NewUserState} = Module:ssactor_conversation_error(Protocol, Role, Err, UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
% Setup successful
handle_cast(_Msg = {ssa_session_established, Protocol, Role, CID}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  ConvKey = {Protocol, Role, CID, State#actor_state.monitor_pid},
  {ok, NewUserState} =
    Module:ssactor_conversation_established(Protocol, Role, CID,
                                            ConvKey, UserState),
  {noreply, State#actor_state{user_state=NewUserState}};
% Conversation ended
handle_cast({conversation_ended, CID, Reason}, State) ->
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  {ok, NewUserState} = Module:ssactor_conversation_ended(CID, Reason, UserState),
  {noreply, State#actor_state{user_state=NewUserState}};

% In-session synchronous call
handle_cast({ssa_call_req, MonitorPID, ProtocolName, RoleName, ConversationID,
             MsgData, From}, State) ->
  actor_info("Processing synchronous call ~p ~n", [MsgData], State),
  Sender = message:message_sender(MsgData),
  Op = message:message_name(MsgData),
  Payload = message:message_payload(MsgData),
  Module = State#actor_state.actor_type_name,
  UserState = State#actor_state.user_state,
  % Notify monitor that we've started processing the message
  HandleRes = Module:ssactor_handle_call(
                   ProtocolName, RoleName, ConversationID, Sender, Op, Payload, UserState,
                   {ProtocolName, RoleName, ConversationID, State#actor_state.monitor_pid}),
  % Save role state, reset handler state to idle, and grab new user state
  NewUserState =
    case HandleRes of
      {reply, Reply, NewState} ->
        % Send the reply back to the caller
        error_logger:info_msg("Sending reply back to caller~n"),
        actor_monitor:outgoing_call_response(MonitorPID, ProtocolName, RoleName,
                                             ConversationID, Sender, Op,
                                             Reply, From),
        NewState;
      {noreply, NewState} ->
        NewState;
      {stop, NewState} ->
        NewState;
      _Other ->
        exit(wrong_return_value)
    end,
  {noreply, State#actor_state{user_state=NewUserState}};
handle_cast({ssa_subsession_setup_failure, SubsessionName, ProtocolName, RoleName,
             ConvID, Reason}, State) ->
  handle_subsession_setup_failure(SubsessionName, ProtocolName, RoleName,
                                  ConvID, Reason, State);
handle_cast({ssa_subsession_failure, SubsessionName, ProtocolName,
             RoleName, ConvID, FailureName}, State) ->
  handle_subsession_failure(SubsessionName, ProtocolName,
                            RoleName, ConvID, FailureName, State);
handle_cast({ssa_subsession_success, SubsessionName, ProtocolName,
             RoleName, ConvID, Result}, State) ->
  handle_subsession_success(SubsessionName, ProtocolName, RoleName,
                            ConvID, Result, State);
handle_cast(Msg, State) ->
  delegate_async(handle_cast, Msg, State).


% Info messages -- we don't do anything with these
handle_info(Msg, State) ->
  delegate_async(handle_info, Msg, State).

% We don't need this.
code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  actor_error("Actor terminating for reason ~p~n", [Reason], State),
  Module = State#actor_state.actor_type_name,
  MonitorPID = State#actor_state.monitor_pid,
  UserState = State#actor_state.user_state,
  actor_registry:deregister_actor(Module, MonitorPID),
  Module:terminate(Reason, UserState),
  ok.

% Internal API

message(ActorPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:cast(ActorPID, {ssa_msg, ProtocolName, RoleName, ConvID, Msg}).

conversation_success(ActorPID, ProtocolName, RoleName, ConvID) ->
  gen_server2:cast(ActorPID, {ssa_session_established, ProtocolName, RoleName, ConvID}).

incoming_call_request(ActorPID, ProtocolName, RoleName, ConvID, MonitorPID, Message, From) ->
  gen_server2:cast(ActorPID, {ssa_call_req, MonitorPID, ProtocolName, RoleName, ConvID,
             Message, From}).

join_conversation_request(ActorPID, ProtocolName, RoleName, ConvID) ->
  %io:format("JCR: Actor: ~p, PN: ~p, RN: ~p, CID: ~p~n",
  %          [ActorPID, ProtocolName, RoleName, ConvID]),
  gen_server2:call(ActorPID, {ssa_join_conversation, ProtocolName, RoleName, ConvID}).

subsession_setup_failure(ActorPID, SubsessionName, ProtocolName, RoleName, ConvID, Reason) ->
  gen_server2:cast(ActorPID, {ssa_subsession_setup_failure, SubsessionName,
                              ProtocolName, RoleName, ConvID, Reason}).

subsession_failure(ActorPID, SubsessionName, ProtocolName, RoleName, ConvID, FailureName) ->
  gen_server2:cast(ActorPID, {ssa_subsession_failure, SubsessionName,
                              ProtocolName, RoleName, ConvID, FailureName}).

subsession_success(ActorPID, SubsessionName, ProtocolName,
                   RoleName, ConvID, SubsessionResult) ->
  gen_server2:cast(ActorPID, {ssa_subsession_success, SubsessionName,
                              ProtocolName, RoleName, ConvID, SubsessionResult}).



%%%%%%%%%%%%%
%%%% API %%%%
%%%%%%%%%%%%%

conversation_ended(ActorPID, CID, Reason) ->
  gen_server2:cast(ActorPID, {conversation_ended, CID, Reason}).

call(ServerRef, Message) ->
  gen_server2:call(ServerRef, Message).

call(ServerRef, Message, Timeout) ->
  gen_server2:call(ServerRef, Message, Timeout).

cast(ServerRef, Message) ->
  gen_server2:cast(ServerRef, Message).

reply(ServerRef, Message) ->
  gen_server2:reply(ServerRef, Message).

start_link(ModuleName, Args, Options) ->
  actor_monitor:start_link(ModuleName, Args, Options).
%  io:format("SSA Gen server start called for ~p~n", [ModuleName]),
%  Res = gen_server2:start_link(ssa_gen_server, [ModuleName, Args], Options),
%  unwrap_start_result(Res).

start_link(RegName, ModuleName, Args, Options) ->
  actor_monitor:start_link(RegName, ModuleName, Args, Options).
%  Res = gen_server2:start_link(ssa_gen_server, [RegName, ModuleName, Args], Options),
%  unwrap_start_result(Res).

start(ModuleName, Args, Options) ->
  actor_monitor:start_link(ModuleName, Args, Options).
%  Res = gen_server2:start(ssa_gen_server, [ModuleName, Args], Options),
%  unwrap_start_result(Res).

start(RegName, ModuleName, Args, Options) ->
  actor_monitor:start_link(RegName, ModuleName, Args, Options).

%  Res = gen_server2:start(ssa_gen_server, [RegName, ModuleName, Args], Options),
%  unwrap_start_result(Res).

start_actor_process(ModuleName, Args, MonitorPID) ->
  gen_server2:start_link(ssa_gen_server, [ModuleName, Args, MonitorPID], []).

