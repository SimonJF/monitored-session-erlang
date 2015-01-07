% List of nonterminals.
Nonterminals
payloadtypename
modulename
protocoldecl
roleinstantiationlist
localcatches
messagesignature
globalprotocoldecl
annotationname
recursionlabelname
module
globalchoiceinner
localprotocolinstance
roleinstantiationlistinner
globalprotocolinstance
parametername
rolename
message
payload
localdo
argumentlist
localprotocolheader
globalinterruptible
globalmessagetransfer
rolenamelist
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
scopename
globalcontinue
argument
messagelist
globalprotocolheader
payloadtypedecl
moduledecl
localcatch
protocolname
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
simplemembername.



% List of terminals.
Terminals
module_kw import_kw type_kw protocol_kw global_kw local_kw role_kw
sig_kw instantiates_kw as_kw from_kw to_kw choice_kw at_kw or_kw
rec_kw continue_kw par_kw and_kw interruptible_kw with_kw by_kw
throws_kw catches_kw do_kw left_brace right_brace left_bracket
right_bracket left_square_bracket right_square_bracket colon forward_slash
back_slash dot hash ampersand question_mark exlamation_mark underscore
comma semicolon less_than greater_than.


% Module is the root symbol.
Rootsymbol module.

% Primitives
empty -> '$empty'.
rolename -> ident : unwrap('$1').
payloadtypename -> ident : unwrap('$1').
protocolname -> ident : unwrap('$1').
parametername -> ident : unwrap('$1').
annotationname -> ident : unwrap('$1').
recursionlabelname -> ident : unwrap('$1').
scopename -> ident : unwrap('$1').

% Module names
modulename -> ident module_ident_inners dot ident : '$1' ++ '$2' ++ '$4'.
modulename -> ident : unwrap('$1').

module_ident_inner -> dot ident : unwrap('$2').
module_ident_inners -> empty : "".
module_ident_inners -> module_ident_inner module_ident_inners : "." ++ '$1' ++ '$2'

% Member names: either simple or dotted strings, basically
membername -> simplemembername : '$1'.
membername -> fullmembername : '$1'.

simplemembername -> payloadtypename : unwrap('$1').
simplemembername -> protocolname : unwrap('$1').

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

moduledecl -> module_kw modulename semicolon : unwrap('$2').

importdecl -> importmodule : '$1'.
importdecl -> importmember : '$1'.

importmodule -> import_kw modulename semicolon : scribble_ast:module_import('$2').
importmodule -> import_kw modulename as_kw ident semicolon :
  scribble_ast:module_import('$2', '$3').

importmember -> from_kw modulename import_kw simplemembername semicolon :
  scribble_ast:member_import('$2', '$4').
importmember -> from_kw modulename import_kw simplemembername as_kw ident semicolon :
  scribble_ast:member_import('$2', '$4', unwrap('$6')).

payloadtypedecl -> type_kw less_than ident greater_than ext_ident from_kw ext_ident as_kw payloadtypename semicolon :
  Type = unwrap('$3'),
  ExternalName = unwrap('$5'),
  TypeSource = unwrap('$7'),
  TypeName = unwrap('$9'),
  scribble_ast:payload_type(Type, ExternalName, TypeSource, TypeName).

messagesignature -> left_bracket payload right_bracket:
  message_signature_payload('$2').
messagesignature -> ident left_bracket payloads right_bracket :
  message_signature(unwrap('$1'), '$2').

payloads -> payload : ['$1'].
payloads -> payload payloads : ['$1'|'$2'].

payload -> payloadelement payloadelementlist : ['$1'|'$2'].
payloadelementlist -> empty : [].
payloadelementlist -> comma payloadelement payloadelementlist : ['$2'|'$3'].

payloadelement -> payloadtypename : '$1'.
payloadelement -> parametername : '$1'.
payloadelement -> parametername : '$1'.
payloadelement -> annotationname colon payloadtypename: '$1' ++ ":" ++ '$3'.
payloadelement -> annotationname colon parametername : '$1' ++ ":" ++ '$3'.

protocoldecl -> globalprotocoldecl : '$1'.
protocoldecl -> localprotocoldecl : '$1'.

globalprotocoldecl -> globalprotocolheader globalprotocoldefinition :
  {Name, Params, Roles} = '$1',
  scribble_ast:global_protocol(Name, Params, Roles, '$2').

globalprotocoldecl -> globalprotocolheader globalprotocolinstance :
  {Name, Params, Roles} = '$1',
  {InstProt, Args, InstRoles} = '$2',
  scribble_ast:global_protocol_instance(Name, Params, Roles, InstProt, Args, InstRoles).

globalprotocolheader -> global_kw protocol_kw protocolname roledecllist:
  {'$3', [], '$4'}.
globalprotocolheader -> global_kw protocol_kw protocolname parameterdecllist roledecllist:
  {'$3', '$4', '$5'}.

roledecllist -> left_bracket roledecl roledecllistinner right_bracket.
roledecllistinner -> empty : [].
roledecllistinner -> comma roledecl roledecllistinner : ['$2'|'$3'].

roledecl -> role_kw rolename : role_decl('$2').
roledecl -> role_kw rolename as_kw rolename : role_decl('$2', '$4').


parameterdecllist -> less_than parameterdecl parameterdecllistinner greater_than :
  ['$2'|'$3'].
parameterdecllistinner -> empty : [].
parameterdecllistinner -> comma parameterdecl parameterdecllistinner : ['$2'|'$3'].

parameterdecl -> type_kw parametername : scribble_ast:type_parameter('$2').
parameterdecl -> type_kw parametername as_kw parametername :
  scribble_ast:type_parameter('$2', '$4').
parameterdecl -> sig_kw parametername : scribble_ast:sig_parameter('$2').
parameterdecl -> sig_kw parametername as_kw parametername :
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


roleinstantiation -> rolename : scribble_ast:role_instantiation('$1').
roleinstantiation -> rolename as_kw rolename : scribble_ast:role_instantiation('$1', '$3').

argumentlist -> less_than argument argumentlistinner greater_than :
  ['$1'|'$2'].
argumentlistinner -> empty : [].
argumentlistinner -> comma argument argumentlistinner : ['$2'|'$3'].

argument -> messagesignature : scribble_ast:arg_message_sig('$1').
argument -> messagesignature as_kw parametername:
  scribble_ast:arg_message_sig('$1', '$3').
argument -> payloadtypename : scribble_ast:arg_payload_type('$1').
argument -> payloadtypename as_kw parametername:
  scribble_ast:arg_payload_type('$1', '$2').
argument -> parametername: scribble_ast:arg_parameter('$1').
argument -> parametername as_kw parametername:
  scribble_ast:arg_parameter('$1', '$2').

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

globalmessagetransfer -> message from_kw rolename to_kw rolename rolenamelist semicolon:
  scribble_ast:message_transfer('$1', '$3', ['$5'|'$6']).
rolenamelist -> empty : [].
rolenamelist -> comma rolename rolenamelist : ['$2'|'$3'].

message -> messagesignature : '$1'.
message -> parametername : scribble_ast:message_signature('$1', []).

globalchoice -> choice_kw at_kw rolename globalprotocolblock globalchoiceinner :
  scribble_ast:choice('$3', ['$4'|'$5']).
globalchoiceinner -> empty : [].
globalchoiceinner -> or_kw globalprotocolblock globalchoiceinner : ['$2'|'$3'].

globalrecursion -> rec_kw recursionlabelname globalprotocolblock:
  scribble_ast:recursion('$2', '$3').

globalcontinue -> continue_kw recursionlabelname semicolon:
  scribble_ast:continue('$2', '$3').

globalparallel -> par_kw globalprotocolblock globalparallelinner :
  scribble_ast:parallel(['$2'|'$3']).
globalparallelinner -> empty : [].
globalparallelinner -> and_kw globalprotocolblock globalparallelinner : ['$2'|'$3'].


globalinterruptible -> interruptible_kw globalprotocolblock with_kw left_brace globalinterruptlist right_brace:
  scribble_ast:interruptible('$2', '$5').
globalinterruptible -> interruptible_kw scopename colon globalprotocolblock with_kw left_brace globalinterruptlist right_brace:
  scribble_ast:interruptible('$2', '$4', '$7').

globalinterruptlist -> empty : [].
globalinterruptlist -> globalinterrupt globalinterruptlist : ['$1'|'$2'].

messagelist -> empty : [].
messagelist -> comma message messagelist : ['$2'|'$3'].

globalinterrupt -> message messagelist by_kw rolename semicolon:
  scribble_ast:interrupt('$2', '$4').

globaldo -> do_kw membername roleinstantiationlist semicolon:
  scribble_ast:do('$2', [], '$3').
globaldo -> do_kw membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do('$2', '$3', '$4').

globaldo -> do_kw scopename colon membername roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5').
globaldo -> do_kw scopename colon membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5', '$6').

localprotocoldecl -> localprotocolheader localprotocoldefinition:
  {Name, ProjRoleName, Params, Roles} = '$1',
  scribble_ast:local_protocol(Name, ProjRoleName, Params, Roles, Interactions).
localprotocoldecl -> localprotocolheader localprotocolinstance

localprotocolheader -> local_kw protocol_kw protocolname at_kw rolename roledecllist:
  {'$3', '$5', [], '$6'}.
localprotocolheader -> local_kw protocol_kw protocolname at_kw rolename parameterdecllist roledecllist:
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


localsend -> message to_kw rolename rolenamelist semicolon:
  scribble_ast:local_send('$1', ['$3'|'$4']).

localreceive -> message from_kw ident semicolon:
  scribble_ast:local_receive('$1', unwrap('$3')).

localchoice -> choice_kw at_kw rolename localprotocolblock localchoicelist:
  scribble_ast:choice('$3', ['$4'|'$5']).
localchoicelist -> empty : [].
localchoicelist -> or_kw localprotocolblock localchoicelist : ['$2'|'$3'].


localrecursion -> rec_kw recursionlabelname localprotocolblock
  scribble_ast:recursion('$2', '$3').

localcontinue -> continue_kw recursionlabelname semicolon
  scribble_ast:continue('$1').

localparallel -> par_kw localprotocolblock localparallelinner :
  scribble_ast:parallel(['$2'|'$3']).
localparallelinner -> empty : [].
localparallelinner -> and_kw localprotocolblock localparallelinner : ['$2'|'$3'].


localinterruptible -> interruptible_kw scopename colon localprotocolblock with_kw left_brace localcatches right_brace:
  scribble_ast:local_interruptible('$2', '$4', '$7').
localinterruptible -> interruptible_kw scopename colon localprotocolblock with_kw left_brace localthrow localcatches right_brace
  scribble_ast:local_interruptible('$2', '$4', '$7', '$8').

localcatches -> empty : [].
localcatches -> localcatch localcatches : ['$1'|'$2'].

localthrow -> throws_kw message messagelist to_kw rolename rolenamelist semicolon:
  local_throw(['$2'|'$3'], ['$5'|'$6']).

localcatch -> catches_kw message messagelist from_kw rolename semicolon:
  local_catch(['$2'|'$3'], '$5').

localdo -> do_kw membername roleinstantiationlist semicolon:
  scribble_ast:do('$2', [], '$3').
localdo -> do_kw membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do('$2', '$3', '$4').

localdo -> do_kw scopename colon membername roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5').
localdo -> do_kw scopename colon membername argumentlist roleinstantiationlist semicolon:
  scribble_ast:do_scope('$2', '$4', '$5', '$6').


Erlang code.

unwrap({_, V}) -> V.
unwrap({_, _, V}) -> V.
unwrap(V) -> ct:print("Cannot unwrap token ~p", [V]).
