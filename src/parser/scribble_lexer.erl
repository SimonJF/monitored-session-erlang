-module(scribble_lexer).
-compile(export_all).
-import(leex, [file/2]).

main() ->
  leex:file('scribble_tokens', []).
