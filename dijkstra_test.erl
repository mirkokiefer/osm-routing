-module(dijkstra_test).

-export([setup/0, test/2, test_all/0, test_short/0]).

setup() ->
  processing:loadData(),
  _Http = server:start().


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
  Johannes = '26999575',
  Haykuhi = '1118741072',
  Mirko = '60191569',
  Feld = '275283807',
  MannheimWasserturm = '534690910',
  B = '366201864',
  test(Mirko, Haykuhi).