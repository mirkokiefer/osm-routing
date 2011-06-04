-module(dijkstra).
-export([shortest_path/2]).

shortest_path(SourceID, TargetID) ->
  Tab = ets:new(shortest_path, [set, public]),
  VisitedNodes = ets:new(visited_nodes, [set, public]),
  Queue = gb_trees:empty(),
  Q1 = updateDistance(SourceID, undefined, 0, {Tab, Queue}),
  recurseNodes(SourceID, TargetID, Q1, {Tab, VisitedNodes}).
  
recurseNodes(Target, Target, _Queue, {Tab, _VisitedNodes}) ->
  io:format("found target: ~p~n", [Target]),
  Tab;

recurseNodes(Node, Target, Queue, {Tab, VisitedNodes}) ->
  visited(Node, true, VisitedNodes),
  Q1 = priority_queue:remove({Node, getDistance(Node, Tab)}, Queue),
  Neighbours = geodata:edges(Node),
  Q2 = updateDistances(Node, Neighbours, {Tab, Q1, VisitedNodes}),
  case gb_trees:size(Q2) of
    0 -> Tab;
    _Any -> {ClosestNode, _Distance} = priority_queue:smallest(Q2),
      recurseNodes(ClosestNode, Target, Q2, {Tab, VisitedNodes})
  end.

updateDistance(Node, OldDistance, NewDistance, {Tab, Queue}) ->
  Q1 = priority_queue:remove({Node, OldDistance}, Queue),
  Q2 = priority_queue:add({Node, NewDistance}, Q1),
  ets:insert(Tab, {Node, {new_d, NewDistance}}),
  Q2.
  
getDistance(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, {new_d, Distance}}] -> Distance;
    [] -> undefined
  end.
  
notVisited(Node, VisitedNodes) ->
  case ets:lookup(VisitedNodes, Node) of
    [{Node, true}] -> false;
    [] -> true
  end.
  
visited(Node, Bool, VisitedNodes) ->
  ets:insert(VisitedNodes, {Node, Bool}).

updateDistances(CurrentNode, Neighbours, {Tab, Queue, VisitedNodes}) ->
  CurrentDistance = getDistance(CurrentNode, Tab),
  recurseNeighbours(Neighbours, CurrentDistance, {Tab, Queue, VisitedNodes}).

recurseNeighbours([], _CurrentDistance, {_Tab, Queue, _VisitedNodes}) ->
  Queue;

recurseNeighbours([Neighbour|Rest], CurrentDistance, {Tab, Queue, VisitedNodes}) ->
  {{node, Node}, {distance, Distance}} = Neighbour,
  %io:format("n: ~p, current: ~p~n", [Node, CurrentDistance]),
  NewDistance = CurrentDistance + Distance,
  OldDistance = getDistance(Node, Tab),
  NotVisited = notVisited(Node, VisitedNodes),
  NewQueue = if (NewDistance < OldDistance) and NotVisited ->
    updateDistance(Node, OldDistance, NewDistance, {Tab, Queue});
    true -> Queue
  end,
  recurseNeighbours(Rest, CurrentDistance, {Tab, NewQueue, VisitedNodes}).
  