
-module(astar_priority_queue_test).

-include_lib("eunit/include/eunit.hrl").


add_remove_get_smallest_test() ->
  List = [{abc, 1}, {ab, 2}, {de, 4}],
  Q1 = astar_priority_queue:add(List, gb_trees:empty()),
  
  ?assertEqual(List, astar_priority_queue:list(Q1)),
  
  ?assertEqual({abc, 1}, astar_priority_queue:smallest(Q1)),
  
  Q2 = astar_priority_queue:remove({abc, 1}, Q1),
  ?assertEqual([{ab, 2}, {de, 4}], astar_priority_queue:list(Q2)),
  
  Q3 = astar_priority_queue:remove_all([{ab, 2}, {de, 4}], Q2),
  ?assertEqual([], astar_priority_queue:list(Q3)).