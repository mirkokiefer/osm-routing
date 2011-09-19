-module(priority_queue).

-export([new/0, add/2, list/1, remove/2, remove_all/2, smallest/1]).

new() ->
  gb_trees:empty().

add({Element, Value}, Queue) ->
  gb_trees:insert({Value, Element}, null, Queue);

add([{Element, Value}|Rest], Queue) ->
  NewQueue = gb_trees:insert({Value, Element}, null, Queue),
  add(Rest, NewQueue);
  
add([], Queue) ->
  Queue.
  
remove({Element, Value}, Queue) ->
  gb_trees:delete_any({Value, Element}, Queue).
  
remove_all([Entry|Rest], Queue) ->
  NewQueue = remove(Entry, Queue),
  remove_all(Rest, NewQueue);
  
remove_all([], Queue) ->
  Queue.
  
list(Queue) ->
  [{Element, Value} || {{Value, Element}, _} <- gb_trees:to_list(Queue)].
  
smallest(Queue) ->
  {{Value, Element}, _} = gb_trees:smallest(Queue),
  {Element, Value}.