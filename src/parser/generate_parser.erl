-module(generate_parser).
-compile(export_all).
-import(yecc, [file/2]).

main() ->
  yecc:file('scribble_parser', [{verbose, false}]).
