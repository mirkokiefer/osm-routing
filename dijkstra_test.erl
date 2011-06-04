-module(dijkstra_test).

-export([setup/0, test/2, test_all/0, test_short/0]).

setup() ->
  compile:file('dijkstra'),
  processing:loadData().

test(A, B) ->
  StartT = now(),
  dijkstra:shortest_path(A, B),
  EndT = now(),
  timer:now_diff(EndT, StartT).
  
  
test_all() ->
  A = '33811020',
  B = 'abc',
  test(A, B).
  
test_short() ->
  A = '33811019',
  B = '26999535',
  test(A, B).