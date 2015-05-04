-module(role_monitor).
-behaviour(gen_server2).
-compile(export_all).
% A monitor for a role in a given conversation.
% Created by a conversation instance, and persists through the lifetime
% of the *conversation* as opposed to the *actor*.

-record(role_monitor_state, {actor_pid, % PID of the actor_proxy
                             protocol_name, % Name of the protocol
                             role_name, % Name of the role
                             conversation_id, % PID of the conversation
                             role_state, % Per-role state, independent of actor
                             handler_state, % Current state of message handling:
                                            % Either idle, handler_started, message_sent
                             monitor}). % Monitor FSM


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Logging Functions
log_msg(Func, Format, Args, State) ->
  InfoStr = "Actor PID ~p, monitor instance ~p.",
  InfoArgs = [State#role_monitor_state.actor_pid,
              self()],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

% Warn function. It was ad-hoc, horrific, and verbose before, so standardise it.
monitor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

% Error function. Same as warn, really
monitor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

monitor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).


% Here, we queue the message, pending commit once all parties have received it
queue_incoming_message(Msg, State) ->
  ActorPid = State#role_monitor_state.actor_pid,
  ProtocolName = State#role_monitor_state.protocol_name,
  RoleName = State#role_monitor_state.role_name,
  ConversationID = State#role_monitor_state.conversation_id,
  actor_proxy:queue_message(ActorPid, ProtocolName, RoleName,
                            ConversationID, Msg).


% "Commit" the message, sending it to the program logic
commit_incoming_message(MsgRef, State) ->
  ActorPID = State#role_monitor_state.actor_pid,
  actor_proxy:deliver_message(ActorPID, self(), MsgRef).

% Drop the message from the queue
drop_incoming_message(MsgRef, State) ->
  ActorPID = State#role_monitor_state.actor_pid,
  actor_proxy:drop_message(ActorPID, MsgRef).


report_monitor_fail(MessageData, CommType, Err, State) ->
   monitor_warn("Monitor failed when processing message ~p (~p). Error: ~p~n",
                [MessageData, CommType, Err], State).

%deliver_outgoing_message(Msg, State) ->
%  ProtocolName = State#role_monitor_state.protocol_name,
%  RoleName = State#role_monitor_state.role_name,
%  ConversationID = State#role_monitor_state.conversation_id,
%  gen_server2:call(ConversationID, {outgoing_msg, ProtocolName, RoleName, Msg}).

% Handles an incoming message. Checks whether we're in the correct conversation,
% then grabs the monitor, then checks / updates the monitor state.
handle_incoming_message(MessageData, State) ->
  monitor_info("Handling incoming message ~p", [MessageData], State),
  Res = monitor_msg(recv, MessageData, State),
  case Res of
    {ok, NewState} ->
      queue_incoming_message(MessageData, NewState),
      {ok, NewState};
    Err -> {Err, State} % assuming the error has been logged already
  end.


handle_incoming_call_resp(MessageData, State) ->
  monitor_info("Handling incoming message ~p", [MessageData], State),
  Res = monitor_msg(recv_call_resp, MessageData, State),
  case Res of
    {ok, NewState} -> {ok, NewState};
    Err -> {Err, State} % assuming the error has been logged already
  end.


handle_incoming_call_req(MessageData, State, From) ->
  Res = monitor_msg(recv_call_req, MessageData, State),
  case Res of
    {ok, NewState} ->
      Res2 = deliver_call_request(MessageData, NewState, From),
      if Res2 == ok ->
          {ok, NewState};
         Res2 =/= ok ->
          % Actor has died; rollback the monitor, call off the dawn
          {{error, dead_actor}, State}
      end;
    Err ->
      monitor_info("Monitor failed:~p~n", [Err], State),
      % TODO: Here, notify sender that the monitor has failed.
      {Err, State}
  end.

deliver_call_request(Message, State, From) ->

  % Grab everything that we need from the state
  ActorPID = State#role_monitor_state.actor_pid,
  ProtocolName = State#role_monitor_state.protocol_name,
  RoleName = State#role_monitor_state.role_name,
  ConversationID = State#role_monitor_state.conversation_id,

  % Check whether the actor's alive, and deliver the call request if so
  IsAlive = actor_proxy:is_actor_alive(ActorPID),
  if IsAlive ->
      actor_proxy:deliver_call_request(ActorPID, self(), ProtocolName, RoleName,
                                       ConversationID, Message, From),
      ok;
     not IsAlive ->
       error
  end.



%%% Handles an outgoing message, by monitoring and sending to the conversation
%%% instance for routing if all is well.
handle_outgoing_message(MessageType, Message, State) ->
  % Construct a message instance, send to the monitor, and check the result
  MonitorRes = monitor_msg(MessageType, Message, State),
  case MonitorRes of
    {ok, NewState} ->
       % Return ok, indicating the monitor's fine, and store the new monitor
       % Mark that a message has been sent in this handler
       NewState1 = NewState#role_monitor_state{handler_state=message_sent},
       {ok, NewState1};
    Err ->
       report_monitor_fail(Message, MessageType, Err, State),
       {error, Err}
  end.

% Performs the actual monitoring.
monitor_msg(CommType, MessageData, State) ->
  MonitorFunction =
    case CommType of
      send -> fun monitor:send/2;
      recv -> fun monitor:recv/2;
      send_call_req -> fun monitor:send_call_request/2;
      recv_call_req -> fun monitor:recv_call_request/2;
      send_call_resp -> fun monitor:send_call_response/2;
      recv_call_resp -> fun monitor:recv_call_response/2
    end,
  Monitor = State#role_monitor_state.monitor,
  MonitorResult = MonitorFunction(MessageData, Monitor),
  case MonitorResult of
    {ok, NewMonitorInstance} ->
      NewState = State#role_monitor_state{monitor=NewMonitorInstance},
      {ok, NewState};
    {error, Err, _Monitor} -> {error, Err}
  end.

with_actor_pid(Fun, State) ->
  ActorPID = State#role_monitor_state.actor_pid,
  if ActorPID =/= undefined ->
       Fun(ActorPID);
     true -> {error, undefined_actor_pid}
  end.

set_handler_state_finished(State) ->
  NewState = State#role_monitor_state{handler_state=idle},
  {reply, ok, NewState}.
set_handler_state_finished(State, NewRoleState) ->
  NewState = State#role_monitor_state{handler_state=idle,
                                      role_state=NewRoleState},
  {reply, ok, NewState}.

set_handler_started(State) ->
  NewState = State#role_monitor_state{handler_state=handler_started},
  {reply, ok, NewState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ProtocolName, RoleName, ConversationPid, Monitor]) ->
  State = #role_monitor_state{actor_pid=undefined,
                              protocol_name=ProtocolName,
                              role_name=RoleName,
                              conversation_id=ConversationPid,
                              handler_state=idle,
                              monitor=Monitor},
  {ok, State}.

handle_call({send_ssa_message, Msg},
            _From, State) ->
  % Handle the actor sending a message
  {Reply, NewState} = handle_outgoing_message(send, Msg, State),
  {reply, Reply, NewState};
handle_call({send_call_request, Msg},
            _From, State) ->
  % Handle the actor sending a message
  {Reply, NewState} = handle_outgoing_message(send_call_req, Msg, State),
  {reply, Reply, NewState};
handle_call({send_call_response, Msg},
            _From, State) ->
  % Handle the actor sending a message
  {Reply, NewState} = handle_outgoing_message(send_call_resp, Msg, State),
  {reply, Reply, NewState};
handle_call({recv_call_request, Msg, From},
            _From, State) ->
  % Handle the actor sending a message
  {Reply, NewState} = handle_incoming_call_req(Msg, State, From),
  {reply, Reply, NewState};
handle_call({recv_call_response, Msg},
            _From, State) ->
  % Handle the actor sending a message
  {Reply, NewState} = handle_incoming_call_resp(Msg, State),
  {reply, Reply, NewState};
handle_call({receive_ssa_message, Msg}, _From, State) ->
  % Handle the actor receiving a message
  {Reply, NewState} = handle_incoming_message(Msg, State),
  {reply, Reply, NewState};
handle_call({set_actor_pid, Pid}, _From, State) ->
  % Change the actor ID.
  NewState = State#role_monitor_state{actor_pid=Pid},
  {reply, ok, NewState};
handle_call(is_actor_alive, _From, State) ->
  ActorPID = State#role_monitor_state.actor_pid,
  Res = actor_proxy:is_actor_alive(ActorPID),
  {reply, Res, State};
handle_call(handler_started, _From, State) ->
  set_handler_started(State);
handle_call(handler_finished, _From, State) ->
  set_handler_state_finished(State);
handle_call({handler_finished, NewRoleState}, _From, State) ->
  set_handler_state_finished(State, NewRoleState);
handle_call(Other, _From, State) ->
  error_logger:warn_msg("Unhandled call in role_monitor ~p: ~p~n", [self(), Other]),
  {noreply, State}.

handle_cast(conversation_success, State) ->
  EndedFun =
    fun (ActorProxyPID) ->
        ProtocolName = State#role_monitor_state.protocol_name,
        RoleName = State#role_monitor_state.role_name,
        ConversationID = State#role_monitor_state.conversation_id,
        actor_proxy:conversation_success(ActorProxyPID, ProtocolName, RoleName,
                                         ConversationID) end,
  with_actor_pid(EndedFun, State),
  {noreply, State};
handle_cast({conversation_ended, Reason}, State) ->
  EndedFun =
    fun (ActorProxyPID) ->
        ConversationID = State#role_monitor_state.conversation_id,
        actor_proxy:conversation_ended(ActorProxyPID, ConversationID, Reason) end,
  with_actor_pid(EndedFun, State),
  {noreply, State};
handle_cast({commit_message, MsgRef}, State) ->
  commit_incoming_message(MsgRef, State),
  {noreply, State};
handle_cast({drop_message, MsgRef}, State) ->
  drop_incoming_message(MsgRef, State),
  {noreply, State};
handle_cast(Other, State) ->
  error_logger:warning_msg("Unhandled cast message in role_monitor: ~p~n", [Other]),
  {noreply, State}.

handle_info(_Other, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_actor_pid(MonitorPID, ActorPID) ->
  gen_server2:call(MonitorPID, {set_actor_pid, ActorPID}).

send_message(MonitorPID, Message) ->
  gen_server2:call(MonitorPID, {send_ssa_message, Message}).

receive_message(MonitorPID, Message) ->
  gen_server2:call(MonitorPID, {receive_ssa_message, Message}).

commit_message(MonitorPID, MsgRef) ->
  gen_server2:cast(MonitorPID, {commit_message, MsgRef}).

drop_message(MonitorPID, MsgRef) ->
  gen_server2:cast(MonitorPID, {drop_message, MsgRef}).

send_call_request(MonitorPID, Message) ->
  gen_server2:call(MonitorPID, {send_call_request, Message}).

receive_call_request(MonitorPID, Message, From) ->
  gen_server2:call(MonitorPID, {recv_call_request, Message, From}).

send_call_response(MonitorPID, Message) ->
  gen_server2:call(MonitorPID, {send_call_response, Message}).

receive_call_response(MonitorPID, Message) ->
  gen_server2:call(MonitorPID, {recv_call_response, Message}).

start_link(ProtocolName, RoleName, ConversationID, MonitorFSM) ->
  gen_server2:start_link(role_monitor, [ProtocolName, RoleName, ConversationID, MonitorFSM], []).

conversation_ended(MonitorPID, Reason) ->
  gen_server2:cast(MonitorPID, {conversation_ended, Reason}).

conversation_success(MonitorPID, _CID) ->
  gen_server2:cast(MonitorPID, conversation_success).

is_actor_alive(MonitorPID) ->
  gen_server2:call(MonitorPID, is_actor_alive).

handler_started(MonitorPID) ->
  gen_server2:call(MonitorPID, handler_started).

handler_finished(MonitorPID) ->
  gen_server2:call(MonitorPID, handler_finished).

handler_finished(MonitorPID, NewRoleState) ->
  gen_server2:call(MonitorPID, {handler_finished, NewRoleState}).
