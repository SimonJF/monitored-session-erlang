-module(protocol_loader).
-export([load_protocol_files/1]).

% Loads protocols from a given directory.

% Scribble files can contain multiple protocols.
% ScribbleAST -> [{ProtocolName, RoleProjName, ProtocolAST}]
extract_protocols({module, _, _, _, Protocols}) ->
  lists:filtermap(fun(Protocol) ->
              case Protocol of
                {local_protocol, ProtocolName, ProjRoleName, _, _, _} ->
                  {true, {ProtocolName, ProjRoleName, Protocol}};
                _Other ->
                  error_logger:warning_msg("WARN: Couldn't extract protocol of wrong type ~p~n",
                                           [Protocol]),
                  false
              end end, Protocols);
extract_protocols(Other) ->
  error_logger:warning_msg("WARN: Scribble file without top-level module, ignoring: ~p~n", [Other]),
  [].


append_protocol_roles(ProtocolEntries, Dict) ->
  lists:foldl(fun({ProtocolName, RoleName, ProtocolAST}, RunningPRD) ->
                  % First, get the ProtocolName |-> RoleSpec dict
                  io:format("Storing role ~s to rolespec ~p mapping ~n", [RoleName, ProtocolAST]),
                  ProtocolRoleDictRes = orddict:find(ProtocolName, RunningPRD),
                  case ProtocolRoleDictRes of
                    {ok, ProtocolRoleDict} ->
                      % Now, update the PRD with the new role.
                      NewPRD = orddict:store(RoleName, ProtocolAST, ProtocolRoleDict),
                      orddict:store(ProtocolName, NewPRD, RunningPRD);
                    error ->
                      % Otherwise, create a new one and store it
                      NewPRD = orddict:store(RoleName, ProtocolAST, orddict:new()),
                      orddict:store(ProtocolName, NewPRD, RunningPRD)
                  end end, Dict, ProtocolEntries).



% Traverses the spec directory.
% Produces a ProtocolName |-> (Role |-> RoleSpec) mapping, where RoleSpec is
% the parsed AST of the local protocol
load_protocol_files(SpecDir) ->
  % I'm going to take advantage of Let It Fail here
  % Assume that the directory exists -- otherwise the program shouldn't
  % run.
  {ok, Filenames} = file:list_dir(SpecDir),
  ScribbleFiles = lists:filter(fun(Filename) -> util:string_contains(Filename, ".scr") end,
                              Filenames),

  lists:foldl(fun (Filename, Dict) ->
    % Now, try and parse:
    ParseRes = scribble_lexer:parse(util:prefix_dir_name(SpecDir, Filename)),
    case ParseRes of
      {ok, AST} -> ExtractResult = extract_protocols(AST),
                   append_protocol_roles(ExtractResult, Dict);
      _Other ->
        error_logger:warning_msg("WARN: Could not parse file ~s: ignoring~n", [Filename]),
        Dict
    end end, orddict:new(), ScribbleFiles).


