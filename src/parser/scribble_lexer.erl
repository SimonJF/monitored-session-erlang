-module(scribble_lexer).
-compile(export_all).
-import(file, [read_file/1]).

% Lex and parse a file
parse(Filename) ->
  ScribbleFile = util:open_file(Filename),
  % Open the file
  case ScribbleFile of
    {error, Reason} -> {error, file_open_error, Reason};
    {ok, FileStr} ->
      % Lexing time
      LexRes = scribble_tokens:string(FileStr),
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
