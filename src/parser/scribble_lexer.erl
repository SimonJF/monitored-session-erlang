-module(scribble_lexer).
-compile(export_all).
-import(file, [read_file/1]).
-define(SCRIBBLE_FILE, "bbs.scr").

% Reads a file and converts it from binary to text
open_file(Filename) ->
  OpenResult = file:read_file(Filename),
  case OpenResult of
    {error, Reason} -> {error, Reason};
    {ok, Binary} -> {ok, unicode:characters_to_list(Binary)}
  end.

% Test harness
parse(Filename) ->
  ScribbleFile = open_file(Filename),
  % Open the file
  case ScribbleFile of
    {error, Reason} -> {error, file_open_error, Reason};
    {ok, FileStr} ->
      % Lexing time
      LexRes = scribble_tokens:string(FileStr),
      io:format("Tokens: ~p~n", [LexRes]),
      case LexRes of
        {ok, Tokens, _EndLine} ->
          ParseRes = scribble_parser:parse(Tokens),
          % Finally, parse!
          case ParseRes of
            {ok, Res} -> {ok, Res};
            {error, Err, _} -> {error, parse_error, Err};
            Other -> {error, parse_error, Other}
          end;
        {error, Error} -> {error, lex_error, Error}
      end
  end.
