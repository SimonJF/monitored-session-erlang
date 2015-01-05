% Leex Lexer for Scribble.

Definitions.

% Whitespace
%(\t | \s | \r | \n)+
WHITESPACE = [\t\s\r\n]+
%COMMENT = /\* .* \*/
%LINE_COMMENT = // [^\n\r]* \r? \n

% Primitives
LETTER = [a-zA-Z]
DIGIT = [0-9]
UNDERSCORE = _
%SYMBOL = { | } | \( | \) | \[ | \] | : | / | \\ | \. | \# | & | \? | \! | UNDERSCORE

% Identifiers
%IDENTIFIER = (LETTER | UNDERSCORE) (LETTER | DIGIT | UNDERSCORE)*
%EXTIDENTIFIER =	" ( [^'\\''"'] )* "

% Reserved keywords
MODULEKW = module
IMPORTKW = import
TYPEKW = type
PROTOCOLKW = protocol
GLOBALKW = global
LOCALKW = local
ROLEKW = role
SIGKW = sig
INSTANTIATESKW = instantiates
ASKW = as
FROMKW = from
TOKW = to
CHOICEKW = choice
ATKW = at
%ORKW = or
RECKW = rec
CONTINUEKW = continue
PARKW = par
%ANDKW = and
INTERRUPTIBLEKW = interruptible
WITHKW = with
BYKW = by
THROWSKW = throws
CATCHESKW = catches
DOKW = do

% --- Lexer Rules ---

Rules.

% Keywords
{MODULEKW} : {token, {module_kw, TokenLine}}.
{IMPORTKW} : {token, {import_kw, TokenLine}}.
{TYPEKW} : {token, {type_kw, TokenLine}}.
{PROTOCOLKW} : {token, {protocol_kw, TokenLine}}.
{GLOBALKW} : {token, {global_kw, TokenLine}}.
{LOCALKW} : {token, {local_kw, TokenLine}}.
{ROLEKW} : {token, {role_kw, TokenLine}}.
{SIGKW} : {token, {sig_kw, TokenLine}}.
{INSTANTIATESKW} : {token, {instantiates_kw, TokenLine}}.
{ASKW} : {token, {as_kw, TokenLine}}.
{FROMKW} : {token, {from_kw, TokenLine}}.
{TOKW} : {token, {to_kw, TokenLine}}.
{CHOICEKW} : {token, {choice_kw, TokenLine}}.
{ATKW} : {token, {at_kw, TokenLine}}.
{ORKW} : {token, {or_kw, TokenLine}}.
{RECKW} : {token, {rec_kw, TokenLine}}.
{CONTINUEKW} : {token, {continue_kw, TokenLine}}.
{PARKW} : {token, {par_kw, TokenLine}}.
{ANDKW} : {token, {and_kw, TokenLine}}.
{INTERRUPTIBLEKW} : {token, {interruptible_kw, TokenLine}}.
{WITHKW} : {token, {with_kw, TokenLine}}.
{BYKW} : {token, {by_kw, TokenLine}}.
{THROWSKW} : {token, {throws_kw, TokenLine}}.
{CATCHESKW} : {token, {catches_kw, TokenLine}}.
{DOKW} : {token, {do_kw, TokenLine}}.

% Identifiers
{IDENTIFIER} : {token, {ident, TokenLine, TokenChars}}.

Erlang code.

