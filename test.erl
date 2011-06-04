-module(test).

-export([setup/0, test/0]).



setup() ->
  compile:file('dijkstra'),
  processing:run().

test() ->
  dijkstra_test:test().