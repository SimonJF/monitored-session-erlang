-module(generate_lexer).
-compile(export_all).
-import(leex, [file/2]).

% Generates the Scribble Lexer using Leex and the token definition
% file given in scribble_tokens

main() ->
  leex:file('scribble_tokens', []).
