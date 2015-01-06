% Erlang translation of the Scribble AST.

-module(scribble_ast).
-compile(export_all).

% Top-level module
module(Name, Imports, Payloads, Protocols) ->
  {module, Name, Imports, Payloads, Protocols}.

% Payload Type
payload_type(Type, ExternalName, TypeSource, Name) ->
  {payload_type, Type, ExternalName, TypeSource, Name}.

% Global protocols
global_protocol(Name, Roles, Interactions) ->
  {global_protocol, Name, Roles, Interactions}.

message_transfer(SenderRole, ReceiverRoles) ->
  {message_transfer, SenderRole, ReceiverRoles}.

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

% An interrupt consists of multiple messages from a given participant
% which can interrupt the current flow, meaning that execution continues
% outside of the scope
interrupt(Messages, ByRole) ->
  {interrupt, Messages, ByRole}.

% Interesting way of doing subprotocol calls...
do(Name, RoleInstantiations) ->
  {do, Name, RoleInstantiations}.
