-module(scribble_lexer).
-compile(export_all).
-import(file, [read_file/1]).
-import(scribble_tokens, [string/1]).
-import(scribble_parser, [parse/1]).
-define(SCRIBBLE_FILE, "bbs.scr").

% Reads a file and converts it from binary to text
open_file(Filename) ->
  OpenResult = file:read_file(Filename),
  case OpenResult of
    {error, Reason} -> {error, Reason};
    {ok, Binary} -> {ok, unicode:characters_to_list(Binary)}
  end.


% Test harness
main() ->
  ScribbleFile = open_file(?SCRIBBLE_FILE),
  case ScribbleFile of
    {error, Reason} -> io:format("Error opening file ~s, error: ~s", [?SCRIBBLE_FILE, Reason]);
    {ok, FileStr} ->
      io:format("Read file: ~s, with content ~s", [?SCRIBBLE_FILE, FileStr]),
      LexRes = scribble_tokens:string(FileStr),
      case LexRes of
        {ok, Tokens, _EndLine} ->
          io:format("Lexed successfully...~n"),
          io:format("~p~n", [Tokens]),
          Res = scribble_parser:parse(Tokens),
          io:format("Parsed successfully! ~p~n", [Res]);
        {error, Error} -> io:format("Lex error ~p~n", [Error]);
        _Other -> io:format("Lex error ~n", [])
      end
  end.

bobsleigh() ->
  {ok, pines}.
