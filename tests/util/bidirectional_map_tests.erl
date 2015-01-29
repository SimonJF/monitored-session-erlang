-module(bidirectional_map_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

add_retrieve_test() ->
  Map = bidirectional_map:new(),
  Map1 = bidirectional_map:store("Hello", "World", Map),
  Map2 = bidirectional_map:store("Hi", "Universe", Map1),
  "World" = bidirectional_map:fetch_left("Hello", Map2),
  "Hi" = bidirectional_map:fetch_right("Universe", Map2),
  ok.

add_erase_test() ->
  Map = bidirectional_map:new(),
  Map1 = bidirectional_map:store("Hello", "World", Map),
  Map2 = bidirectional_map:store("Hi", "Universe", Map1),
  Map3 = bidirectional_map:remove_left("Hello", Map2),
  ?assertError(function_clause, bidirectional_map:fetch_left("Hello", Map3)),
  ?assertError(function_clause, bidirectional_map:fetch_right("World", Map3)),
  "Universe" = bidirectional_map:fetch_left("Hi", Map3),
  "Hi" = bidirectional_map:fetch_right("Universe", Map3),
  ok.
