-module(bidirectional_map).
-compile(export_all).

% A bidirectional map. Useful when wanting to look up things by both
% the keys and values. Implemented as two unidirectional maps with a
% suitable abstraction layer.
% Requires keys and values to be unique, of course: no key may be the
% same as a value, and vice versa. Also no values may be the same as
% each other. Pretty restrictive data structure, but should be ok for
% quick 'n' dirty uses.

new() -> {orddict:new(), orddict:new()}.

store(Left, Right, Map = {_LeftMap, _RightMap}) ->
  {LM, RM} = remove_left(Left, Map),
  LM1 = orddict:store(Left, Right, LM),
  RM1 = orddict:store(Right, Left, RM),
  {LM1, RM1}.

fetch_left(Left, {LeftMap, _}) ->
  orddict:fetch(Left, LeftMap).

fetch_right(Right, {_, RightMap}) ->
  orddict:fetch(Right, RightMap).

find_left(Left, {LeftMap, _}) ->
  orddict:find(Left, LeftMap).

find_right(Right, {_, RightMap}) ->
  orddict:find(Right, RightMap).

remove_left(Left, {LeftMap, RightMap}) ->
  LeftMap1 = orddict:erase(Left, LeftMap),
  RightMap1 = orddict:filter(fun(_, V) -> V =/= Left end, RightMap),
  {LeftMap1, RightMap1}.

remove_right(Right, {LeftMap, RightMap}) ->
  LeftMap1 = orddict:filter(fun(_, V) -> V =/= Right end, LeftMap),
  RightMap1 = orddict:erase(Right, RightMap),
  {LeftMap1, RightMap1}.

contains_left(Left, {LeftMap, _}) ->
  orddict:is_key(Left, LeftMap).

contains_right(Right, {_, RightMap}) ->
  orddict:is_key(Right, RightMap).

from_orddict(Orddict) ->
  orddict:fold(fun(K, V, BiMap) -> store(K, V, BiMap) end,
               new(), Orddict).

