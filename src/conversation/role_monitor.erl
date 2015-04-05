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

% Warn function. It was ad-hoc, horrific, and verbos before, so standardise it.
monitor_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

% Error function. Same as warn, really
monitor_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

monitor_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).


% Here, we deliver the message to the attached actor (which is a gen_server2).
deliver_incoming_message(Msg, State) ->
  ActorPid = State#role_monitor_state.actor_pid,
  ProtocolName = State#role_monitor_state.protocol_name,
  RoleName = State#role_monitor_state.role_name,
  ConversationID = State#role_monitor_state.conversation_id,
  gen_server2:cast(ActorPid, {ssa_msg, ProtocolName, RoleName, ConversationID, Msg}).

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
    {ok, NewState} -> {ok, NewState};
    Err -> {Err, State} % assuming the error has been logged already
  end.


%%% Handles an outgoing message, by monitoring and sending to the conversation
%%% instance for routing if all is well.
handle_outgoing_message(Message, State) ->
  % Construct a message instance, send to the monitor, and check the result
  Role = State#role_monitor_state.role_name,
  MonitorRes = monitor_msg(send, Message, State).

% Performs the actual monitoring.
monitor_msg(CommType, MessageData, State) ->
  MonitorFunction = case CommType of
                     send -> fun monitor:send/2;
                     recv -> fun monitor:recv/2
                   end,
  Monitor = State#role_monitor_state.monitor,
  MonitorResult = MonitorFunction(MessageData, Monitor),
  case MonitorResult of
    {ok, NewMonitorInstance} ->
      NewState = State#role_monitor_state{monitor=NewMonitorInstance},
      % Do delegation stuff here
      % If we're sending, then pop it to the conversation instance process
      % If we're receiving, then delegate to user session actor code.
      case CommType of
        send ->
          % Return ok, indicating the monitor's fine, and store the new monitor
          {ok, NewState};
        recv ->
          deliver_incoming_message(MessageData, NewState),
          {ok, NewState}
      end;
    {error, Err, _Monitor} ->
      monitor_warn("Monitor failed when processing message ~p (~p). Error: ~p~n",
                   [MessageData, CommType, Err], State),
      {error, Err}
  end.

with_actor_pid(Fun, State) ->
  ActorPID = State#role_monitor_state.actor_pid,
  if ActorPID =/= undefined ->
       Fun(ActorPID);
     true -> {error, undefined_actor_pid}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ProtocolName, RoleName, ConversationPid, Monitor]) ->
  State = #role_monitor_state{actor_pid=undefined,
                              protocol_name=ProtocolName,
                              role_name=RoleName,
                              conversation_id=ConversationPid,
                              monitor=Monitor},
  {ok, State}.

handle_call({send_ssa_message, Msg},
            _From, State) ->
  % Handle the actor sending a message
  {Reply, NewState} = handle_outgoing_message(Msg, State),
  {reply, Reply, NewState};
handle_call({receive_ssa_message, Msg}, _From, State) ->
  % Handle the actor receiving a message
  {Reply, NewState} = handle_incoming_message(Msg, State),
  {reply, Reply, NewState};
handle_call({set_actor_pid, Pid}, _From, State) ->
  % Change the actor ID.
  NewState = State#role_monitor_state{actor_pid=Pid},
  {reply, ok, NewState}.

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

start_link(ProtocolName, RoleName, ConversationID, MonitorFSM) ->
  gen_server2:start_link(role_monitor, [ProtocolName, RoleName, ConversationID, MonitorFSM], []).

conversation_ended(MonitorPID, Reason) ->
  gen_server2:cast(MonitorPID, {conversation_ended, Reason}).

conversation_success(MonitorPID, CID) ->
  gen_server2:cast(MonitorPID, conversation_success).

