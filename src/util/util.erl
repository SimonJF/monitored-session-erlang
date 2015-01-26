-module(util).
-compile(export_all).

% Utility functions -- for example, opening files

% Reads a file and converts it from binary to text
open_file(Filename) ->
  OpenResult = file:read_file(Filename),
  case OpenResult of
    {error, Reason} -> {error, Reason};
    {ok, Binary} -> {ok, unicode:characters_to_list(Binary)}
  end.

string_contains(String, Segment) ->
  string:str(String, Segment) > 0.


prefix_dir_name("", Filename) -> Filename;
prefix_dir_name(DirName, Filename) ->
  filename:join(DirName, Filename).
