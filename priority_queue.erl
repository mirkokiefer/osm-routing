-module(priority_queue).

-export([add/2, list/1, remove/2, smallest/1]).

add([{Element, Value}|Rest], Queue) ->
  NewQueue = gb_trees:insert({Value, Element}, null, Queue),
  add(Rest, NewQueue);
  
add([], Queue) ->
  Queue.
  
remove({Element, Value}, Queue) ->
  gb_trees:delete_any({Value, Element}, Queue).
  
list(Queue) ->
  [{Element, Value} || {{Value, Element}, _} <- gb_trees:to_list(Queue)].
  
smallest(Queue) ->
  {{Value, Element}, _} = gb_trees:smallest(Queue),
  {Element, Value}.