% List of nonterminals.
Nonterminals
identifier
modulename
protocoldecl
roleinstantiationlist
localcatches
messagesignature
globalprotocoldecl
module
globalchoiceinner
localprotocolinstance
roleinstantiationlistinner
globalprotocolinstance
message
payload
localdo
argumentlist
localprotocolheader
globalinterruptible
globalmessagetransfer
identifierlist
globaldo
roledecllistinner
parameterdecllistinner
localprotocolblock
localchoice
payloadelementlist
importmember
membername
localinterruptible
globalinterruptlist
localinteraction
parameterdecl
importdecls
empty
payloads
localprotocoldecl
globalprotocoldefinition
localthrow
globalprotocolblock
parameterdecllist
argumentlistinner
importmodule
globalcontinue
argument
messagelist
globalprotocolheader
payloadtypedecl
moduledecl
localcatch
localprotocoldefinition
payloadelement
importdecl
globalparallel
globalchoice
localparallelinner
localcontinue
localreceive
globalinteraction
protocoldecls
localparallel
globalinterrupt
localinteractionsequence
payloadtypedecls
roleinstantiation
localchoicelist
roledecllist
globalinteractionsequence
globalparallelinner
localsend
fullmembername
globalrecursion
localrecursion
roledecl
simplemembername
module_ident_inner
module_ident_inners
ext_identifier
localinvites
localsendcallreq
localrecvcallreq
localsendcallresp
localrecvcallresp
localinitiates
localinitiatesone
handleblock
handleblocks.




% List of terminals.
Terminals
module_kw import_kw type_kw protocol_kw global_kw local_kw role_kw sig_kw
instantiates_kw as_kw from_kw to_kw choice_kw at_kw or_kw rec_kw continue_kw
par_kw and_kw interruptible_kw with_kw by_kw throws_kw catches_kw do_kw
left_brace right_brace left_bracket right_bracket left_square_bracket
right_square_bracket colon forward_slash back_slash dot hash ampersand
question_mark exlamation_mark underscore comma semicolon less_than greater_than
ident ext_ident transient_kw invitation_kw for_kw send_call_request_kw
send_call_response_kw receive_call_request_kw receive_call_response_kw handle_kw
initiates_kw new_kw.


% Module is the root symbol.
Rootsymbol module.

% Primitives
empty -> '$empty'.
identifier -> ident : unwrap('$1').
ext_identifier -> ext_ident : unwrap('$1').

% Module names
modulename -> identifier module_ident_inners : '$1' ++ '$2'.

module_ident_inner -> dot identifier : "." ++ '$2'.
module_ident_inners -> empty : "".
module_ident_inners -> module_ident_inner module_ident_inners : '$1' ++ '$2'.

% Member names: either simple or dotted strings, basically
membername -> simplemembername : '$1'.
membername -> fullmembername : '$1'.

simplemembername -> identifier : '$1'.

fullmembername -> modulename dot simplemembername : '$1' ++ "." ++ '$3'.

% Module: Top level thing which is parsed.
% Contains a name, then three classes of things: import declarations,
% payload type declarations, and protocol declarations.
module -> moduledecl importdecls payloadtypedecls protocoldecls :
  scribble_ast:module('$1', '$2', '$3', '$4').

importdecls -> empty : [].
importdecls -> importdecl importdecls : ['$1'|'$2'].

payloadtypedecls -> empty : [].
payloadtypedecls -> payloadtypedecl payloadtypedecls : ['$1'|'$2'].

protocoldecls -> empty : [].
protocoldecls -> protocoldecl protocoldecls : ['$1'|'$2'].

moduledecl -> module_kw modulename semicolon : '$2'.

importdecl -> importmodule : '$1'.
importdecl -> importmember : '$1'.

importmodule -> import_kw modulename semicolon : scribble_ast:module_import('$2').
importmodule -> import_kw modulename as_kw identifier semicolon :
  scribble_ast:module_import('$2', '$3').

importmember -> from_kw modulename import_kw simplemembername semicolon :
  scribble_ast:member_import('$2', '$4').
importmember -> from_kw modulename import_kw simplemembername as_kw identifier semicolon :
  scribble_ast:member_import('$2', '$4', '$6').

payloadtypedecl -> type_kw less_than identifier greater_than ext_identifier from_kw ext_identifier as_kw identifier semicolon :
  Type = '$3',
  ExternalName = '$5',
  TypeSource = '$7',
  TypeName = '$9',
  scribble_ast:payload_type(Type, ExternalName, TypeSource, TypeName).

messagesignature -> left_bracket payload right_bracket:
  scribble_ast:message_signature("", ['$2']).
messagesignature -> identifier left_bracket right_bracket :
  scribble_ast:message_signature('$1', []).
messagesignature -> identifier left_bracket payload right_bracket :
  scribble_ast:message_signature('$1', '$3').

%payloads -> payload : ['$1'].
%payloads -> payload payloads : ['$1'|'$2'].

payload -> payloadelement payloadelementlist : ['$1'|'$2'].
payloadelementlist -> empty : [].
payloadelementlist -> comma payloadelement payloadelementlist : ['$2'|'$3'].

payloadelement -> identifier : '$1'.
payloadelement -> identifier colon identifier : '$1' ++ ":" ++ '$3'.

protocoldecl -> globalprotocoldecl : '$1'.
protocoldecl -> localprotocoldecl : '$1'.

globalprotocoldecl -> globalprotocolheader globalprotocoldefinition :
  {Name, Params, Roles} = '$1',
  scribble_ast:global_protocol(Name, Params, Roles, '$2').

globalprotocoldecl -> globalprotocolheader globalprotocolinstance :
  {Name, Params, Roles} = '$1',
  {InstProt, Args, InstRoles} = '$2',
  scribble_ast:global_protocol_instance(Name, Params, Roles, InstProt, Args, InstRoles).

globalprotocolheader -> global_kw protocol_kw identifier roledecllist:
  {'$3', [], '$4'}.
globalprotocolheader -> global_kw protocol_kw identifier parameterdecllist roledecllist:
  {'$3', '$4', '$5'}.

roledecllist -> left_bracket roledecl roledecllistinner right_bracket : ['$2'|'$3'].
roledecllistinner -> empty : [].
roledecllistinner -> comma roledecl roledecllistinner : ['$2'|'$3'].

roledecl -> role_kw identifier : scribble_ast:role_decl('$2').
roledecl -> role_kw identifier as_kw identifier : scribble_ast:role_decl('$2', '$4').
roledecl -> transient_kw role_kw identifier : scribble_ast:transient_role_decl('$3').
roledecl -> transient_kw role_kw identifier as_kw identifier : scribble_ast:transient_role_decl('$3', '$5').

parameterdecllist -> less_than parameterdecl parameterdecllistinner greater_than :
  ['$2'|'$3'].
parameterdecllistinner -> empty : [].
parameterdecllistinner -> comma parameterdecl parameterdecllistinner : ['$2'|'$3'].

parameterdecl -> type_kw identifier : scribble_ast:type_parameter('$2').
parameterdecl -> type_kw identifier as_kw identifier :
  scribble_ast:type_parameter('$2', '$4').
parameterdecl -> sig_kw identifier : scribble_ast:sig_parameter('$2').
parameterdecl -> sig_kw identifier as_kw identifier :
  scribble_ast:sig_parameter('$2', '$4').

globalprotocoldefinition -> globalprotocolblock : '$1'.


globalprotocolinstance -> instantiates_kw membername roleinstantiationlist semicolon:
  {'$2', [], '$3'}.
globalprotocolinstance -> instantiates_kw membername argumentlist roleinstantiationlist semicolon:
  {'$2', '$3', '$4'}.

roleinstantiationlist -> left_bracket roleinstantiation roleinstantiationlistinner right_bracket:
  ['$2'|'$3'].
roleinstantiationlistinner -> empty : [].
roleinstantiationlistinner -> comma roleinstantiation roleinstantiationlistinner:
  ['$2'|'$3'].


roleinstantiation -> identifier : scribble_ast:role_instantiation('$1').
roleinstantiation -> new_kw identifier : scribble_ast:new_role_instantiation('$2').
roleinstantiation -> identifier as_kw identifier : scribble_ast:role_instantiation('$1', '$3').

argumentlist -> less_than argument argumentlistinner greater_than :
  ['$1'|'$2'].
argumentlistinner -> empty : [].
argumentlistinner -> comma argument argumentlistinner : ['$2'|'$3'].


% As each argument type is an identifier, we get reduce/reduce conflicts here.
% Temp measure (and it may be that we don't need them anyway...): just treat
% every argument as a payload type name
argument -> messagesignature : scribble_ast:arg_message_sig('$1').
argument -> messagesignature as_kw identifier:
  scribble_ast:arg_message_sig('$1', '$3').
argument -> identifier : scribble_ast:arg_payload_type('$1').
argument -> identifier as_kw identifier:
  scribble_ast:arg_payload_type('$1', '$2').
%argument -> identifier: scribble_ast:arg_parameter('$1').
%argument -> identifier as_kw identifier:
%  scribble_ast:arg_parameter('$1', '$2').

globalprotocolblock -> left_brace globalinteractionsequence right_brace: '$2'.

globalinteractionsequence -> empty: [].
globalinteractionsequence -> globalinteraction globalinteractionsequence: ['$1'|'$2'].

globalinteraction -> globalmessagetransfer : '$1'.
globalinteraction -> globalchoice : '$1'.
globalinteraction -> globalrecursion : '$1'.
globalinteraction -> globalcontinue : '$1'.
globalinteraction -> globalparallel : '$1'.
globalinteraction -> globalinterruptible : '$1'.
globalinteraction -> globaldo : '$1'.

globalmessagetransfer -> message from_kw identifier to_kw identifier identifierlist semicolon:
  scribble_ast:message_transfer('$1', '$3', ['$5'|'$6']).
identifierlist -> empty : [].
identifierlist -> comma identifier identifierlist : ['$2'|'$3'].

message -> messagesignature : '$1'.
message -> identifier : scribble_ast:message_signature('$1', []).

globalchoice -> choice_kw at_kw identifier globalprotocolblock globalchoiceinner :
  scribble_ast:choice('$3', ['$4'|'$5']).
globalchoiceinner -> empty : [].
globalchoiceinner -> or_kw globalprotocolblock globalchoiceinner : ['$2'|'$3'].

globalrecursion -> rec_kw identifier globalprotocolblock:
  scribble_ast:recursion('$2', '$3').

globalcontinue -> continue_kw identifier semicolon:
  scribble_ast:continue('$2').

globalparallel -> par_kw globalprotocolblock globalparallelinner :
  scribble_ast:parallel(['$2'|'$3']).
globalparallelinner -> empty : [].
globalparallelinner -> and_kw globalprotocolblock globalparallelinner : ['$2'|'$3'].


globalinterruptible -> interruptible_kw globalprotocolblock with_kw left_brace globalinterruptlist right_brace:
  scribble_ast:interruptible('$2', '$5').
globalinterruptible -> interruptible_kw identifier colon globalprotocolblock with_kw left_brace globalinterruptlist right_brace:
  scribble_ast:interruptible('$2', '$4', '$7').

globalinterruptlist -> empty : [].
globalinterruptlist -> globalinterrupt globalinterruptlist : ['$1'|'$2'].

messagelist -> empty : [].
messagelist -> comma message messagelist : ['$2'|'$3'].

globalinterrupt -> message messagelist by_kw identifier semicolon:
  scribble_ast:interrupt('$2', '$4').

globaldo -> do_kw membername roleinstantiationlist semicolon:
  scribble_ast:do('$2', [], '$3').
globaldo -> do_kw membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do('$2', '$3', '$4').

globaldo -> do_kw identifier colon membername roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5').
globaldo -> do_kw identifier colon membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5', '$6').

localprotocoldecl -> localprotocolheader localprotocoldefinition:
  {Name, ProjRoleName, Params, Roles} = '$1',
  Interactions = '$2',
  scribble_ast:local_protocol(Name, ProjRoleName, Params, Roles, Interactions).
localprotocoldecl -> localprotocolheader localprotocolinstance:
  {Name, ProjRoleName, Params, Roles} = '$1',
  {InstName, Args, InstList} = '$2',
  scribble_ast:local_protocol_instance(Name, ProjRoleName, Params, Roles, InstName, Args, InstList).

localprotocolheader -> local_kw protocol_kw identifier at_kw identifier roledecllist:
  {'$3', '$5', [], '$6'}.
localprotocolheader -> local_kw protocol_kw identifier at_kw identifier parameterdecllist roledecllist:
  {'$3', '$5', '$6', '$7'}.

localprotocoldefinition -> localprotocolblock : '$1'.

localprotocolinstance -> instantiates_kw membername roleinstantiationlist semicolon:
  {'$2', [], '$3'}.
localprotocolinstance -> instantiates_kw membername argumentlist roleinstantiationlist semicolon:
  {'$2', '$3', '$4'}.
localprotocolblock -> left_brace localinteractionsequence right_brace: '$2'.


localinteractionsequence -> empty : [].
localinteractionsequence -> localinteraction localinteractionsequence : ['$1'|'$2'].

localinteraction -> localsend : '$1'.
localinteraction -> localreceive : '$1'.
localinteraction -> localchoice : '$1'.
localinteraction -> localparallel : '$1'.
localinteraction -> localrecursion : '$1'.
localinteraction -> localcontinue : '$1'.
localinteraction -> localinterruptible : '$1'.
localinteraction -> localdo : '$1'.
localinteraction -> localsendcallreq : '$1'.
localinteraction -> localrecvcallreq : '$1'.
localinteraction -> localsendcallresp : '$1'.
localinteraction -> localrecvcallresp : '$1'.
localinteraction -> localinvites : '$1'.
localinteraction -> localinitiatesone : '$1'.
localinteraction -> localinitiates : '$1'.

localinitiates -> identifier initiates_kw identifier roleinstantiationlist localprotocolblock handleblocks:
  scribble_ast:local_initiates('$1', '$3', '$4', '$5', '$6').

localinitiatesone -> identifier initiates_kw identifier roleinstantiationlist semicolon :
  scribble_ast:local_initiates_one('$1', '$3', '$4').

handleblocks -> empty : [].
handleblocks -> handleblock handleblocks : ['$1'|'$2'].

handleblock -> handle_kw left_bracket identifier right_bracket localprotocolblock :
  scribble_ast:handle_block('$3', '$5').

localsend -> message to_kw identifier identifierlist semicolon:
  scribble_ast:local_send('$1', ['$3'|'$4']).

localreceive -> message from_kw identifier semicolon:
  scribble_ast:local_receive('$1', '$3').

localchoice -> choice_kw at_kw identifier localprotocolblock localchoicelist:
  scribble_ast:choice('$3', ['$4'|'$5']).
localchoicelist -> empty : [].
localchoicelist -> or_kw localprotocolblock localchoicelist : ['$2'|'$3'].


localrecursion -> rec_kw identifier localprotocolblock :
  scribble_ast:recursion('$2', '$3').

localcontinue -> continue_kw identifier semicolon :
  scribble_ast:continue('$2').

localparallel -> par_kw localprotocolblock localparallelinner :
  scribble_ast:parallel(['$2'|'$3']).
localparallelinner -> empty : [].
localparallelinner -> and_kw localprotocolblock localparallelinner : ['$2'|'$3'].


localinterruptible -> interruptible_kw identifier colon localprotocolblock with_kw left_brace localcatches right_brace:
  scribble_ast:local_interruptible('$2', '$4', '$7').
localinterruptible -> interruptible_kw identifier colon localprotocolblock with_kw left_brace localthrow localcatches right_brace:
  scribble_ast:local_interruptible('$2', '$4', '$7', '$8').

localcatches -> empty : [].
localcatches -> localcatch localcatches : ['$1'|'$2'].

localthrow -> throws_kw message messagelist to_kw identifier identifierlist semicolon:
  scribble_ast:local_throw(['$2'|'$3'], ['$5'|'$6']).

localcatch -> catches_kw message messagelist from_kw identifier semicolon:
  scribble_ast:local_catch(['$2'|'$3'], '$5').

localdo -> do_kw membername roleinstantiationlist semicolon:
  scribble_ast:do('$2', [], '$3').
localdo -> do_kw membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do('$2', '$3', '$4').

localdo -> do_kw identifier colon membername roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5').
localdo -> do_kw identifier colon membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5', '$6').

localinvites -> invitation_kw for_kw identifier localprotocolblock :
  scribble_ast:local_invites('$3', '$4').

localsendcallreq -> send_call_request_kw messagesignature to_kw identifier semicolon:
  scribble_ast:local_call_request_send('$2', '$4').

localrecvcallreq -> receive_call_request_kw messagesignature from_kw identifier semicolon:
  scribble_ast:local_call_request_recv('$2', '$4').

localsendcallresp -> send_call_response_kw messagesignature to_kw identifier semicolon:
  scribble_ast:local_call_response_send('$2', '$4').

localrecvcallresp -> receive_call_response_kw messagesignature from_kw identifier semicolon:
  scribble_ast:local_call_response_recv('$2', '$4').

Erlang code.
unwrap({_, V}) -> V;
unwrap({_, _, V}) -> V;
unwrap(V) -> ct:print("Cannot unwrap token ~p", [V]).
