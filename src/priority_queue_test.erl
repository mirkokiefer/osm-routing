
-module(priority_queue_test).

-include_lib("eunit/include/eunit.hrl").


add_remove_get_smallest_test() ->
  List = [{abc, 1}, {ab, 2}, {de, 4}],
  Q1 = priority_queue:add(List, gb_trees:empty()),
  
  ?assertEqual(List, priority_queue:list(Q1)),
  
  ?assertEqual({abc, 1}, priority_queue:smallest(Q1)),
  
  Q2 = priority_queue:remove({abc, 1}, Q1),
  ?assertEqual([{ab, 2}, {de, 4}], priority_queue:list(Q2)),
  
  Q3 = priority_queue:remove_all([{ab, 2}, {de, 4}], Q2),
  ?assertEqual([], priority_queue:list(Q3)).