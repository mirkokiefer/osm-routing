-module(test).

-export([setup/0, test/2, test_all/0, test_short/0]).

setup() ->
  processing:loadData(),
  _Http = server:start().


test(A, B) ->
  StartT = now(),
  astar:shortest_path(A, B),
  EndT = now(),
  timer:now_diff(EndT, StartT).
  
  
test_all() ->
  A = '33811020',
  B = 'abc',
  test(A, B).
  
test_short() ->
  Haykuhi = '1118741072',
  Johannes = '26999575',
  Mirko = '60191569',
  Feld = '275283807',
  MannheimWasserturm = '534690910',
  B = '366201864',
  test(Mirko, Haykuhi).
  
  % http://127.0.0.1:2904/route?source=26999575&target=1118741072
  % http://127.0.0.1:2904/map?source=1118741072&target=60191569
  % http://127.0.0.1:2904/map?source=1118741072&target=26999575
  % http://127.0.0.1:2904/map?source=26999575&target=60191569  
  % http://127.0.0.1:2904/map?source=26999575&target=275283807 
  % geodata:route_annotated('1118741072', '60191569')
  % geodata:route_description('1118741072', '60191569')
  % astar:shortest_path_with_distances('1118741072', '60191569')
  % requests:route_description('1118741072', '60191569')