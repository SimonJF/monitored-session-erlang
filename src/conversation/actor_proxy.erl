-module(actor_proxy).

-behaviour(gen_server2).
-compile(export_all).
-import(util, [list_contains/2]).
-record(proxy_state, {actor_pid, % PID of the attached actor
                      actor_type_name, % Name of the attached actor type
                      active_protocols, % [{ProtocolName, RoleName, ConversationID}]
                      protocol_role_map, % Protocol Name |-> [Role]
                      registered_become_conversations, % Atom |-> Conversation
                      queued_messages % Message ref |-> Message tuple
                                      % Used for 2-phase commit
                    }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Description
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file contains state that is specific to an actor partaking in different
% sessions. Importantly it doesn't contain monitor state, as this is handled
% by processes spawned by conversation instances.
% Instead, this file provides a 'link' to the rest of the conversation system,
% contains registration state for distinguished conversations, and state about
% the roles the actor can fulfil.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Logging Functions
log_msg(Func, Format, Args, State) ->
  InfoStr = "Actor PID ~p, proxy instance ~p.",
  InfoArgs = [State#proxy_state.actor_pid,
              self()],
  Func(Format ++ "~n" ++ InfoStr, Args ++ InfoArgs).

% Warn function. It was ad-hoc, horrific, and verbos before, so standardise it.
proxy_warn(Format, Args, State) ->
  log_msg(fun error_logger:warning_msg/2, Format, Args, State).

% Error function. Same as warn, really
proxy_error(Format, Args, State) ->
  log_msg(fun error_logger:error_msg/2, Format, Args, State).

proxy_info(Format, Args, State) ->
  log_msg(fun error_logger:info_msg/2, Format, Args, State).

fresh_state(ActorPid, ActorTypeName, ProtocolRoleMap) ->
  #proxy_state{actor_pid=ActorPid,
              actor_type_name=ActorTypeName, % Name of the attached actor type
              active_protocols=orddict:new(),
              protocol_role_map=ProtocolRoleMap, % Roles for each protocol
              registered_become_conversations=orddict:new(),
              queued_messages=orddict:new()
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
      proxy_warn("Could not load monitor for protocol ~s, role ~s: ~p ",
                   [ProtocolName, Role, Err], State),
      load_protocol_monitors(ProtocolName, Roles, MonitorDict, State)
  end.



% Initialises the basic monitor state with some default values.
init([ActorPid, ActorTypeName, ProtocolRoleMap]) ->
  % Firstly, create a fresh state with all of the information we've been given
  State = fresh_state(ActorPid, ActorTypeName, ProtocolRoleMap),
  {ok, State}.

% Called when we've been invited to fufil a role.
% If we can fulfil the role and haven't already fulfilled the role, then
% load the empty FSM into the monitors list.
% Returns {ok, NewState} if we can fulfil the role, or either:
%   * {error, already_fulfilled} --> if we've already fulfilled the role
%   * {error, cannot_fulfil} --> if this role isn't offered by the actor
add_role(ProtocolName, RoleName, ConversationID, State) ->
  % Firstly, check to see we're registered for the role
  ProtocolRoleMap = State#proxy_state.protocol_role_map,
  ActiveProtocols = State#proxy_state.active_protocols,
  ActorPID = State#proxy_state.actor_pid,
  % First, check whether we can fulfil the role (ie it's contained in the PRM)
  RoleFindRes =
    case orddict:find(ProtocolName, ProtocolRoleMap) of
      {ok, ProtocolRoles} ->
        list_contains(RoleName, ProtocolRoles);
      error -> false
    end,

    % Check to see whether we already have an entry for the protocol-role-cid
    % triple. If not, then we can fulfil it.
    AlreadyFulfilled =
    lists:any(fun(ActiveTuple) ->
                  ActiveTuple == {ProtocolName, {RoleName, ConversationID}} end,
              orddict:to_list(ActiveProtocols)),

    case {RoleFindRes, AlreadyFulfilled} of
    {true, false} ->
      % We can theoretically fulfil it. Just need to ask the actor...
      JoinRequestResult = gen_server2:call(ActorPID,
                                          {ssa_join_conversation,
                                           ProtocolName,
                                           RoleName,
                                           ConversationID}),
      case JoinRequestResult of
        accept ->
          NewActiveProtocols = orddict:append(ProtocolName, {RoleName, ConversationID}, ActiveProtocols),
          % TODO: Try-Catch round this, in case the conversation goes away
          Res = gen_server2:call(ConversationID, {accept_invitation, RoleName}),
          case Res of
            % Now, add the <Protocol, Role, CID> |-> Monitor mapping
            ok -> {ok, State#proxy_state{active_protocols=NewActiveProtocols}};
            {error, Err} -> {error, Err}
          end;
        decline ->
          {error, actor_declined}
      end;
    {_, true} -> {error, already_fulfilled};
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
      proxy_info("Registered for role ~s in protocol ~s.",
                   [RoleName, ProtocolName], State),
      {reply, ok, NewState};
    {error, Err} ->
      proxy_warn("Could not fulfil role ~s in protocol ~s. Error: ~p",
                   [RoleName, ProtocolName, Err], State),
      {reply, {error, Err}, State}
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

% Handles the end of a conversation, by removing references in active protocols
% and registered conversations
handle_conversation_ended(ConversationID, Reason, State) ->
  ActorPID = State#proxy_state.actor_pid,
  {ActiveProtocols, BecomeConvs} = {State#proxy_state.active_protocols,
                                    State#proxy_state.registered_become_conversations},
  NewActiveProtocols = filter_active_protocols(ActiveProtocols, ConversationID),
  NewBecomeConversations = filter_orddict(fun({_, CID}) -> CID =/= ConversationID end,
                                          BecomeConvs),

  NewState = State#proxy_state{active_protocols=NewActiveProtocols,
                               registered_become_conversations=NewBecomeConversations},
  ssa_gen_server:conversation_ended(ActorPID, ConversationID, Reason),
  {noreply, NewState}.

handle_register_conv(ProtocolName, _RoleName, ConversationID, RegAtom, State) ->
  RegisteredConversations = State#proxy_state.registered_become_conversations,
  NewRegisteredConversations = orddict:store(RegAtom, {ProtocolName, ConversationID}, RegisteredConversations),
  NewState = State#proxy_state{registered_become_conversations = NewRegisteredConversations},
  {reply, ok, NewState}.


handle_become(RegAtom, RoleName, Operation, Arguments, State) ->
  RecipientPID = State#proxy_state.actor_pid,
  RegisteredConversations = State#proxy_state.registered_become_conversations,
  CIDRes = orddict:find(RegAtom, RegisteredConversations),
  case CIDRes of
    {ok, {ProtocolName, CID}} ->
      gen_server2:cast(RecipientPID, {become, ProtocolName, RoleName, Operation, Arguments, CID}),
      {reply, ok, State};
    _ ->
      {reply, error, bad_conversation}
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

% Synchronous messages:
%  * Invitation
%  * Termination
handle_call({invitation, ProtocolName, RoleName, ConversationID}, _Sender, State) ->
  handle_invitation(ProtocolName, RoleName, ConversationID, State);
handle_call({send_msg, _CurrentProtocol, CurrentRole, ConversationID, Recipients,
             MessageName, Types, Payload}, _Sender, State) ->
    % Send to the conversation, which will perform monitoring and send if possible.
    Reply = conversation_instance:outgoing_message(CurrentRole, ConversationID, Recipients,
                                                   MessageName, Types, Payload),
  {reply, Reply, State};
handle_call({become, RoleName, RegAtom, Operation, Arguments}, _Sender, State) ->
  handle_become(RegAtom, RoleName, Operation, Arguments, State);
handle_call({send_delayed_invite, ProtocolName, InviteeRoleName, ConversationID, InviteeMonitorPid},
            _Sender, State) ->
  handle_send_delayed_invite(ProtocolName, InviteeRoleName, ConversationID, InviteeMonitorPid, State);
% Delegate directly to handle_call in monitor
handle_call({register_become, RegAtom, ProtocolName, RoleName, ConvID}, _From, State) ->
  handle_register_conv(ProtocolName, RoleName, ConvID, RegAtom, State);
handle_call(Msg, From, State) ->
  ActorPid = State#proxy_state.actor_pid,
  Reply = gen_server2:call(ActorPid, {delegate_call, From, Msg}),
  {reply, Reply, State}.

%handle_call(Other, Sender, State) ->
%  proxy_warn("Received unhandled synchronous message ~p from PID ~p.",
%               [Other, Sender], State),
%  {reply, unhandled, State}.

% Module:handle_cast(Request, State) -> Result
% Only async messages are actually data ones.
% Delivering these, we'll need a conv ID, I think.
%handle_cast({message, ProtocolName, RoleName, ConversationID, MessageData}, State) ->
%  handle_incoming_message(MessageData, ProtocolName, RoleName, ConversationID, State);
handle_cast({conversation_ended, CID, Reason}, State) ->
  handle_conversation_ended(CID, Reason, State);
handle_cast({queue_msg, ProtocolName, RoleName, ConvID, Msg}, State) ->
  MsgRef = message:message_id(Msg),
  QueuedMsgs = State#proxy_state.queued_messages,
  NewQueuedMsgs = orddict:store(MsgRef, {ProtocolName, RoleName,
                                         ConvID, Msg}, QueuedMsgs),
  {noreply, State#proxy_state{queued_messages=NewQueuedMsgs}};
handle_cast({deliver_msg, MsgRef}, State) ->
  ActorPid = State#proxy_state.actor_pid,
  QueuedMsgs = State#proxy_state.queued_messages,
  {ProtocolName, RoleName, ConvID, Msg} = orddict:fetch(MsgRef, QueuedMsgs),
  % Deliver the message, and remove from queue
  ssa_gen_server:message(ActorPid, ProtocolName, RoleName, ConvID, Msg),
  NewQueuedMsgs = orddict:erase(MsgRef, QueuedMsgs),
  {noreply, State#proxy_state{queued_messages=NewQueuedMsgs}};
handle_cast({drop_msg, MsgRef}, State) ->
  QueuedMsgs = State#proxy_state.queued_messages,
  NewQueuedMsgs = orddict:erase(MsgRef, QueuedMsgs),
  {noreply, State#proxy_state{queued_messages=NewQueuedMsgs}};
handle_cast(Other, State) ->
  ActorPid = State#proxy_state.actor_pid,
  gen_server2:cast(ActorPid, Other),
  {noreply, State}.

handle_info(Info, State) ->
  %proxy_warn("Received unhandled info message ~p.", [Info], State),
  ActorPid = State#proxy_state.actor_pid,
  ActorPid ! Info,
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.


%%%%%%%%%%%%%%%%%%%
%% API Functions %%
%%%%%%%%%%%%%%%%%%%

queue_message(ProxyPID, ProtocolName, RoleName, ConvID, Msg) ->
  gen_server2:cast(ProxyPID, {queue_msg, ProtocolName, RoleName, ConvID, Msg}).

deliver_message(ProxyPID, MsgRef) ->
  gen_server2:cast(ProxyPID, {deliver_msg, MsgRef}).

drop_message(ProxyPID, MsgRef) ->
  gen_server2:cast(ProxyPID, {drop_msg, MsgRef}).

register_become(ProxyPID, RegAtom, ProtocolName, RoleName, ConvID) ->
  gen_server2:call(ProxyPID, {register_become, RegAtom, ProtocolName, RoleName, ConvID}).

% Called when conversation setup succeeds
conversation_success(ProxyPID, ProtocolName, RoleName, ConvID) ->
  gen_server2:cast(ProxyPID, {ssa_session_established, ProtocolName, RoleName, ConvID}).

% Called when conversation setup failed, for whatever reason
conversation_setup_failed(ProxyPID, ProtocolName, RoleName, Error) ->
  gen_server2:cast(ProxyPID, {ssa_conversation_setup_failed, ProtocolName, RoleName, Error}).

become(ProxyPID, RegAtom, RoleName, Operation, Arguments) ->
  gen_server2:call(ProxyPID, {become, RoleName, RegAtom, Operation, Arguments}).


invite({ProtocolName, _, ConversationID, ProxyPID}, InviteeProxyPID, InviteeRoleName) ->
  gen_server2:call(ProxyPID, {send_delayed_invite, ProtocolName,
                               InviteeRoleName, ConversationID,
                               InviteeProxyPID}).

send_message({ProtocolName, RoleName, ConversationID, ProxyPID},
             Recipients, MessageName, Types, Payload) ->
  gen_server2:call(ProxyPID,
                  {send_msg, ProtocolName, RoleName, ConversationID,
                   Recipients, MessageName, Types, Payload}).

% Called by the conversation instance to notify the actor that the conversation has died
conversation_ended(ProxyPID, CID, Reason) ->
  gen_server2:cast(ProxyPID, {conversation_ended, CID, Reason}).

start_link(ActorPID, Module, ProtocolRoleMap) ->
  gen_server2:start_link(actor_proxy,
                         [ActorPID, Module, ProtocolRoleMap], []).

start_link(RegName, ActorPID, Module, ProtocolRoleMap) ->
  gen_server2:start_link(RegName, actor_proxy,
                         [ActorPID, Module, ProtocolRoleMap], []).
