% Leex Lexer for Scribble.
% Apologies for the lack of spaces in the macros -- Leex doesn't like this.

Definitions.

% Whitespace
%(\t | \s | \r | \n)+
WHITESPACE = (\t|\s|\r|\n)+
COMMENT = /\*.*\*/
LINE_COMMENT = //[^\n\r]*\r?\n

% Primitives
LETTER = [a-zA-Z]
DIGIT = [0-9]
UNDERSCORE = _

% Symbols
LEFT_BRACE = \{
RIGHT_BRACE = \}
LEFT_BRACKET = \(
RIGHT_BRACKET = \)
LEFT_SQUARE_BRACKET = \[
RIGHT_SQUARE_BRACKET = \]
COLON = :
FORWARD_SLASH = /
BACK_SLASH = \\
DOT = \.
COMMA = ,
HASH = \#
AMPERSAND = \&
QUESTION_MARK = \?
EXLAMATION_MARK = \!
UNDERSCORE = \_
SEMICOLON = ;
LESS_THAN = <
GREATER_THAN = >

SYMBOL = {|}|\(|\)|\[|\]|:|/|\\|\.|\#|&|\?|\!|_

% Identifiers
EXTIDENTIFIER =	"([^\'\"\'])*"
IDENTIFIER = ([a-zA-Z]|_)([a-zA-Z]|[0-9]|_)*

% Reserved keywords
MODULEKW = module
PACKAGEKW = package
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
ORKW = or
RECKW = rec
CONTINUEKW = continue
PARKW = par
ANDKW = and
INTERRUPTIBLEKW = interruptible
WITHKW = with
BYKW = by
THROWSKW = throws
CATCHESKW = catches
DOKW = do
TRANSIENTKW = transient
INVITATIONKW = invitation
FORKW = for
SEND_CALL_REQ = send_call_request
SEND_CALL_RESPONSE = send_call_response
RECV_CALL_REQ = receive_call_request
RECV_CALL_RESPONSE = receive_call_response
SUBSESSIONKW = subsession
NEWKW = new


% --- Lexer Rules ---

Rules.

% Keywords
{MODULEKW} : make_token(module_kw, TokenLine, TokenChars).
{IMPORTKW} : make_token(import_kw, TokenLine, TokenChars).
{TYPEKW} : make_token(type_kw, TokenLine, TokenChars).
{PROTOCOLKW} : make_token(protocol_kw, TokenLine, TokenChars).
{GLOBALKW} : make_token(global_kw, TokenLine, TokenChars).
{LOCALKW} : make_token(local_kw, TokenLine, TokenChars).
{ROLEKW} : make_token(role_kw, TokenLine, TokenChars).
{SIGKW} : make_token(sig_kw, TokenLine, TokenChars).
{INSTANTIATESKW} : make_token(instantiates_kw, TokenLine, TokenChars).
{ASKW} : make_token(as_kw, TokenLine, TokenChars).
{FROMKW} : make_token(from_kw, TokenLine, TokenChars).
{TOKW} : make_token(to_kw, TokenLine, TokenChars).
{CHOICEKW} : make_token(choice_kw, TokenLine, TokenChars).
{ATKW} : make_token(at_kw, TokenLine, TokenChars).
{ORKW} : make_token(or_kw, TokenLine, TokenChars).
{RECKW} : make_token(rec_kw, TokenLine, TokenChars).
{CONTINUEKW} : make_token(continue_kw, TokenLine, TokenChars).
{PARKW} : make_token(par_kw, TokenLine, TokenChars).
{ANDKW} : make_token(and_kw, TokenLine, TokenChars).
{INTERRUPTIBLEKW} : make_token(interruptible_kw, TokenLine, TokenChars).
{WITHKW} : make_token(with_kw, TokenLine, TokenChars).
{BYKW} : make_token(by_kw, TokenLine, TokenChars).
{THROWSKW} : make_token(throws_kw, TokenLine, TokenChars).
{CATCHESKW} : make_token(catches_kw, TokenLine, TokenChars).
{DOKW} : make_token(do_kw, TokenLine, TokenChars).
{TRANSIENTKW} : make_token(transient_kw, TokenLine, TokenChars).
{INVITATIONKW} : make_token(invitation_kw, TokenLine, TokenChars).
{FORKW} : make_token(for_kw, TokenLine, TokenChars).
{SEND_CALL_REQ} : make_token(send_call_request_kw, TokenLine, TokenChars).
{SEND_CALL_RESPONSE} : make_token(send_call_response_kw, TokenLine, TokenChars).
{RECV_CALL_REQ} : make_token(receive_call_request_kw, TokenLine, TokenChars).
{RECV_CALL_RESPONSE} : make_token(receive_call_response_kw, TokenLine, TokenChars).
{SUBSESSIONKW} : make_token(subsession_kw, TokenLine, TokenChars).
{NEWKW} : make_token(new_kw, TokenLine, TokenChars).



% Identifiers
{IDENTIFIER} : make_token(ident, TokenLine, TokenChars).
{EXTIDENTIFIER} : make_token(ext_ident, TokenLine, TokenChars).

% Symbols
{LEFT_BRACE} : make_token(left_brace, TokenLine, TokenChars).
{RIGHT_BRACE} : make_token(right_brace, TokenLine, TokenChars).
{LEFT_BRACKET} : make_token(left_bracket, TokenLine, TokenChars).
{RIGHT_BRACKET} : make_token(right_bracket, TokenLine, TokenChars).
{LEFT_SQUARE_BRACKET} : make_token(left_square_bracket, TokenLine, TokenChars).
{RIGHT_SQUARE_BRACKET} : make_token(right_square_bracket, TokenLine, TokenChars).
{COLON} : make_token(colon, TokenLine, TokenChars).
{FORWARD_SLASH} : make_token(forward_slash, TokenLine, TokenChars).
{BACK_SLASH} : make_token(back_slash, TokenLine, TokenChars).
{DOT} : make_token(dot, TokenLine, TokenChars).
{HASH} : make_token(hash, TokenLine, TokenChars).
{AMPERSAND} : make_token(ampersand, TokenLine, TokenChars).
{QUESTION_MARK} : make_token(question_mark, TokenLine, TokenChars).
{EXLAMATION_MARK} : make_token(exlamation_mark, TokenLine, TokenChars).
{UNDERSCORE} : make_token(underscore, TokenLine, TokenChars).
{COMMA} : make_token(comma, TokenLine, TokenChars).
{SEMICOLON} : make_token(semicolon, TokenLine, TokenChars).
{LESS_THAN} : make_token(less_than, TokenLine, TokenChars).
{GREATER_THAN} : make_token(greater_than, TokenLine, TokenChars).

% Junk
{WHITESPACE} : skip_token.
{COMMENT} : skip_token.
{LINE_COMMENT} : skip_token.


Erlang code.

make_token(Name, Line, Chars) ->
  {token, {Name, Line, Chars}}.
