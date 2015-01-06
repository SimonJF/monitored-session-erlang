
% List of nonterminals.
Nonterminals
rolename payloadtypename protocolname parametername annotationname
recursionlabelname scopename
modulename membername simplemembername fullmembername

% List of terminals.
Terminals
module_kw import_kw type_kw protocol_kw global_kw local_kw role_kw
sig_kw instantiates_kw as_kw from_kw to_kw choice_kw at_kw or_kw
rec_kw continue_kw par_kw and_kw interruptible_kw with_kw by_kw
throws_kw catches_kw do_kw left_brace right_brace left_bracket
right_bracket left_square_bracket right_square_bracket colon forward_slash
back_slash dot hash ampersand question_mark exlamation_mark underscore
comma semicolon less_than greater_than


% Protocol is the root symbol.
Rootsymbol protocol.

% Primitives
empty -> '$empty'
rolename -> ident : '$1'.
payloadtypename -> ident : '$1'.
protocolname -> ident : '$1'.
parametername -> ident : '$1'.
annotationname -> ident : '$1'.
recursionlabelname -> ident : '$1'.
scopename -> ident : '$1'.

% Module names
modulename -> ident module_ident_inners dot ident.
modulename -> ident.

module_ident_inner = dot ident.
module_ident_inners = empty.
module_ident_inners = module_ident_inner module_ident_inners.


membername -> simplemembername.
membername -> fullmembername.

simplemembername -> payloadtypename.
simplemembername -> protocolname.

fullmembername -> modulename dot simplemembername.

module -> moduledecl importdecls payloadtypedecls protocoldecls.

importdecls -> empty.
importdecls -> importdecl importdecls.

payloadtypedecls -> empty.
payloadtypedecls -> payloadtypedecl payloadtypedecls.

protocoldecls -> empty.
protocoldecls -> protocoldecl protocoldecls.

moduledecl -> module_kw modulename semicolon.

importdecl -> importmodule.
importdecl -> importmember.

importmodule -> import_kw modulename semicolon.
importmodule -> import_kw modulename semicolon.
importmodule -> import_kw modulename as_kw ident semicolon.

importmember -> from_kw modulename import_kw simplemembername semicolon.
importmember -> from_kw modulename import_kw simplemembername as_kw ident semicolon.

payloadtypedecl -> type_kw less_than ident greater_than ext_ident from_kw ext_ident as_kw payloadtypename semicolon.

messagesignature -> left_bracket payload right_bracket.
messagesignature -> ident left_bracket payloads right_bracket.
messagesignature -> left_bracket payload right_bracket.
messagesignature -> ident left_bracket payloads right_bracket.

payloads -> payload.
payloads -> payload payloads.

payload -> payloadelement payloadelementlist
payloadelementlist -> empty.
payloadelementlist -> comma payloadelement payloadelementlist.

payloadelement -> payloadtypename.
payloadelement -> parametername.
payloadelement -> parametername.
payloadelement -> annotationname colon payloadtypename.
payloadelement -> annotationname colon parametername.

protocoldecl -> globalprotocoldecl.
protocoldecl -> localprotocoldecl.

globalprotocoldecl -> globalprotocolheader globalprotocoldefinition.
globalprotocoldecl -> globalprotocolheader globalprotocolinstance.

globalprotocolheader -> global_kw protocol_kw protocolname roledecllist.
globalprotocolheader -> global_kw protocol_kw protocolname parameterdecllist roledecllist.

roledecllist -> left_bracket roledecl roledecllistinner right_bracket.
roledecllistinner -> empty
roledecllistinner -> comma roledecl roledecllistinner

roledecl -> role_kw rolename.
roledecl -> role_kw rolename as_kw rolename.


parameterdecllist -> less_than parameterdecl parameterdecllistinner greater_than.
parameterdecllistinner -> empty
parameterdecllistinner -> comma parameterdecl parameterdecllistinner

parameterdecl -> type_kw parametername
parameterdecl -> type_kw parametername as_kw parametername
parameterdecl -> sig_kw parametername
parameterdecl -> sig_kw parametername as_kw parametername

globalprotocoldefinition -> globalprotocolblock.


globalprotocolinstance -> instantiates_kw membername roleinstantiationlist semicolon
globalprotocolinstance -> instantiates_kw membername argumentlist roleinstantiationlist semicolon

roleinstantiationlist -> left_bracket roleinstantiation roleinstantiationlistinner right_bracket
roleinstantiationlistinner -> empty
roleinstantiationlistinner -> comma roleinstantiation roleinstantiationlistinner


roleinstantiation -> rolename
roleinstantiation -> rolename as_kw rolename

argumentlist -> less_than argument argumentlistinner greater_than.
argumentlistinner -> empty
argumentlistinner -> comma argument argumentlistinner

argument -> messagesignature
argument -> messagesignature as_kw parametername
argument -> payloadtypename
argument -> payloadtypename as_kw parametername
argument -> parametername
argument -> parametername as_kw parametername

globalprotocolblock -> left_brace globalinteractionsequence right_brace.

globalinteractionsequence -> empty
globalinteractionsequence -> globalinteraction globalinteractionsequence.

globalinteraction -> globalmessagetransfer
globalinteraction -> globalchoice
globalinteraction -> globalrecursion
globalinteraction -> globalcontinue
globalinteraction -> globalparallel
globalinteraction -> globalinterruptible
globalinteraction -> globaldo

globalmessagetransfer -> message from_kw rolename to_kw rolename rolenamelist semicolon
rolenamelist -> empty
rolenamelist -> comma rolename rolenamelist

message -> messagesignature.
message -> parametername.

globalchoice -> choice_kw at_kw rolename globalprotocolblock globalchoiceinner.
globalchoiceinner -> empty
globalchoiceinner -> or_kw globalprotocolblock globalchoiceinner.

globalrecursion -> rec_kw recursionlabelname globalprotocolblock

globalcontinue -> continue_kw recursionlabelname semicolon

globalparallel -> par_kw globalprotocolblock globalparallelinner.
globalparallelinner -> empty.
globalparallelinner -> and_kw globalprotocolblock globalparallelinner.


globalinterruptible -> interruptible_kw globalprotocolblock with_kw left_brace globalinterruptlist right_brace
globalinterruptible -> interruptible_kw scopename colon globalprotocolblock with_kw left_brace globalinterruptlist right_brace

globalinterruptlist -> empty
globalinterruptlist -> globalinterrupt globalinterruptlist

messagelist -> empty
messagelist -> comma message messagelist

globalinterrupt -> message messagelist by_kw rolename semicolon

globaldo -> do_kw membername roleinstantiationlist semicolon
globaldo -> do_kw membername argumentlist roleinstantiationlist semicolon
globaldo -> do_kw scopename colon membername roleinstantiationlist semicolon
globaldo -> do_kw scopename colon membername argumentlist roleinstantiationlist semicolon

localprotocoldecl -> localprotocolheader localprotocoldefinition
localprotocoldecl -> localprotocolheader localprotocolinstance

localprotocolheader -> local_kw protocol_kw protocolname at_kw rolename roledecllist
localprotocolheader -> local_kw protocol_kw protocolname at_kw rolename parameterdecllist roledecllist

localprotocoldefinition -> localprotocolblock

localprotocolinstance -> instantiates_kw membername roleinstantiationlist semicolon
localprotocolinstance -> instantiates_kw membername argumentlist roleinstantiationlist semicolon

localprotocolblock -> left_brace localinteractionsequence right_brace

localinteractionsequence -> empty
localinteractionsequence -> localinteraction localinteractionsequence

localinteraction -> localsend
localinteraction -> localreceive
localinteraction -> localchoice
localinteraction -> localparallel
localinteraction -> localrecursion
localinteraction -> localcontinue
localinteraction -> localinterruptible
localinteraction -> localdo


localsend -> message to_kw rolename rolenamelist semicolon

localreceive -> message from_kw ident semicolon

localchoice -> choice_kw at_kw rolename localprotocolblock localchoicelist
localchoicelist -> empty
localchoicelist -> or_kw localprotocolblock localchoicelist


localrecursion -> rec_kw recursionlabelname localprotocolblock

localcontinue -> continue_kw recursionlabelname semicolon

localparallel -> par_kw localprotocolblock localparallellist
localparallellist -> empty
localparallellist -> and_kw localprotocolblock localparallellist

localinterruptible -> interruptible_kw scopename colon localprotocolblock with_kw left_brace localcatches right_brace
localinterruptible -> interruptible_kw scopename colon localprotocolblock with_kw left_brace localthrow localcatches right_brace

localcatches -> empty
localcatches -> localcatch localcatches

localthrow -> throws_kw message messagelist to_kw rolename rolenamelist semicolon

localcatch -> catches_kw message messagelist from_kw rolename semicolon


localdo -> do_kw membername roleinstantiationlist semicolon
localdo -> do_kw membername argumentlist roleinstantiationlist semicolon
localdo -> do_kw scopename colon membername roleinstantiationlist semicolon
localdo -> do_kw scopename colon membername argumentlist roleinstantiationlist semicolon
