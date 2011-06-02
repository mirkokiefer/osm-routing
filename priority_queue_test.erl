-module(priority_queue_test).

-include_lib("eunit/include/eunit.hrl").


add_remove_get_smallest_test() ->
  List = [{abc, 1}, {ab, 2}],
  Q1 = priority_queue:add(List, gb_trees:empty()),
  
  ?assertEqual(priority_queue:list(Q1), List),
  
  ?assertEqual(priority_queue:smallest(Q1), {abc, 1}),
  
  Q2 = priority_queue:remove({abc, 1}, Q1),
  ?assertEqual(priority_queue:list(Q2), [{ab, 2}]).