% Erlang translation of the Scribble AST.

-module(scribble_ast).
-compile(export_all).

% Top-level module
module(Name, Imports, Payloads, Protocols) ->
  {module, Name, Imports, Payloads, Protocols}.

module_import(Name) ->
  {module_import, Name}.

module_import(Name, Alias) ->
  {module_import_alias, Name, Alias}.

member_import(ModuleName, MemberName) ->
  {member_import, ModuleName, MemberName}.

member_import(ModuleName, MemberName, MemberAlias) ->
  {member_import_alias, ModuleName, MemberName, MemberAlias}.

% Payload Type
payload_type(Type, ExternalName, TypeSource, Name) ->
  {payload_type, Type, ExternalName, TypeSource, Name}.

% Global protocols
global_protocol(Name, Parameters, Roles, Interactions) ->
  {global_protocol, Name, Parameters, Roles, Interactions}.

global_protocol_instance(Name, Parameters, Roles, InstantiatedProtocol, Arguments, InstantiatedRoles) ->
  {global_protocol_instance, Name, Parameters, Roles, InstantiatedProtocol, Arguments, InstantiatedRoles}.

% Weird message signature without an ident and only one payload
% Collapse this into message sig for now
%message_signature(Payload) ->
%  {message_signature_payload, Payload}.

% Message signature
message_signature(Message, Payloads) ->
  {message_signature, Message, Payloads}.

message_transfer(Message, SenderRole, ReceiverRoles) ->
  {message_transfer, Message, SenderRole, ReceiverRoles}.

role_decl(Role) ->
  {role_decl, Role}.
role_decl(Role, Alias) ->
  {role_decl_alias, Role, Alias}.

transient_role_decl(Role) ->
  {transient_role_decl, Role}.

transient_role_decl(Role, Alias) ->
  {transient_role_decl_alias, Role, Alias}.

role_instantiation(Name) ->
  {role_instantiation, Name}.

new_role_instantiation(Name) ->
  {new_role_instantiation, Name}.

role_instantiation(Name, Alias) ->
  {role_instantiation, Name, Alias}.

type_parameter(Name) ->
  {type_parameter, Name}.

type_parameter(Name, Alias) ->
  {type_parameter_alias, Name, Alias}.

sig_parameter(Name) ->
  {sig_parameter, Name}.

sig_parameter_alias(Name, Alias) ->
  {sig_parameter_alias, Name, Alias}.

arg_message_sig(MessageSig) ->
  {arg_message_sig, MessageSig}.

arg_message_sig(MessageSig, Alias) ->
  {arg_message_sig_alias, MessageSig, Alias}.

arg_payload_type(PayloadType) ->
  {arg_payload_type, PayloadType}.

arg_payload_type(PayloadType, Alias) ->
  {arg_payload_type, PayloadType, Alias}.

arg_parameter(Parameter) ->
  {arg_parameter, Parameter}.

arg_parameter(Parameter, Alias) ->
  {arg_parameter_alias, Parameter, Alias}.

% A choice by the role given by RoleName.
% Choices is a list of the branches which may be taken, where each
% branch is a list of global interactions.
choice(RoleName, Choices) ->
  {choice, RoleName, Choices}.

% Recursion with a recursion variable
recursion(MuName, Interactions) ->
  {rec, MuName, Interactions}.

% Loops to a recursion variable
continue(MuName) ->
  {continue, MuName}.

% Runs ParallelBlocks in parallel, where each entry in ParallelBlocks
% is a list of global interactions.
parallel(ParallelBlocks) ->
  {par, ParallelBlocks}.

% Defines an interruptible scope: InterruptibleBlock is a normal block
% of global interactions, and InterruptList is a list specifying the interrupts
interruptible(InterruptibleBlock, InterruptList) ->
  {interruptible, InterruptibleBlock, InterruptList}.

interruptible(ScopeName, InterruptibleBlock, InterruptList) ->
  {interruptible_named_scope, ScopeName, InterruptibleBlock, InterruptList}.

% An interrupt consists of multiple messages from a given participant
% which can interrupt the current flow, meaning that execution continues
% outside of the scope
interrupt(Messages, ByRole) ->
  {interrupt, Messages, ByRole}.

% Interesting way of doing subprotocol calls...
do(Name, RoleInstantiations) ->
  {do, Name, [], RoleInstantiations}.

do(Name, ArgList, RoleInstantiations) ->
  {do, Name, ArgList, RoleInstantiations}.

do_scope(ScopeName, Name, RoleInstantiations) ->
  {do_scope, ScopeName, Name, [], RoleInstantiations}.

do_scope(ScopeName, Name, ArgList, RoleInstantiations) ->
  {do_scope, ScopeName, Name, ArgList, RoleInstantiations}.


% Local protocol definition
local_protocol(ProtocolName, ProjRoleName, Params, Roles, Interactions) ->
  {local_protocol, ProtocolName, ProjRoleName, Params, Roles, Interactions}.

local_protocol_instance(ProtocolName, ProjRoleName, Params, Roles, InstName, Args, InstList) ->
  {local_protocol_instance, ProtocolName, ProjRoleName, Params, Roles, InstName, Args, InstList}.


% Local sends and receives
local_send(Message, Recipients) ->
  {local_send, Message, Recipients}.

local_receive(Message, Sender) ->
  {local_receive, Message, Sender}.

% Local throws and catches
local_throw(Messages, Recipients) ->
  {local_throw, Messages, Recipients}.

local_catch(Messages, Sender) ->
  {local_catch, Messages, Sender}.

% Local interruptible block
local_interruptible(ScopeName, InterruptibleBlock, LocalCatches) ->
  {local_interruptible, ScopeName, InterruptibleBlock, LocalCatches}.

% With a throw clause
local_interruptible(ScopeName, InterruptibleBlock, LocalThrow, LocalCatches) ->
  {local_interruptible_throw, ScopeName, InterruptibleBlock, LocalThrow, LocalCatches}.

local_invites(Invitee, Interactions) ->
  {local_invites, Invitee, Interactions}.


local_call_request_send(MessageSignature, Recipient) ->
  {local_call_request_send, MessageSignature, Recipient}.

local_call_request_recv(MessageSignature, Recipient) ->
  {local_call_request_recv, MessageSignature, Recipient}.

local_call_response_send(MessageSignature, Sender) ->
  {local_call_response_send, MessageSignature, Sender}.

local_call_response_recv(MessageSignature, Sender) ->
  {local_call_response_recv, MessageSignature, Sender}.

local_initiates(InitiatorRole, SubsessionName, RoleInstantiationList,
                SuccessBlock, HandleBlocks) ->
  {local_initiates, InitiatorRole, SubsessionName, RoleInstantiationList,
   SuccessBlock, HandleBlocks}.

% Initiates where we assume session completes successfully, success block
% is implicitly the continuation.
% As an example, we have
%   X() from A to B;
%   A initiates OtherProtocol(A, B, new C);
%   Y() from B to A;
%   ...
%
% instead of
%
%  X() from A to B;
%  A initiates OtherProtocol(A, B, new C) {
%    Y() from B to A;;
%  }
local_initiates_one(InitiatorRole, SubsessionName, RoleInstantiationList) ->
  {local_initiates_one, InitiatorRole, SubsessionName, RoleInstantiationList}.

handle_block(FailureName, Block) ->
  {handle_block, FailureName, Block}.
