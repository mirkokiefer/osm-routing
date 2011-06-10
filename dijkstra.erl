-module(dijkstra).
-export([shortest_path/2]).

shortest_path(SourceID, TargetID) ->
  StartT = now(),
  Tab = ets:new(shortest_path, [set, public]),
  VisitedNodes = ets:new(visited_nodes, [set, public]),
  Q1 = gb_trees:empty(),
  
  ets:insert(Tab, {SourceID, {previous, undefined}, {distance, 0}}),
  Path = recurseNodes(SourceID, TargetID, Q1, {Tab, VisitedNodes}),
  
  Distance = getDistance(TargetID, Tab),
  EndT = now(),
  Time = timer:now_diff(EndT, StartT),
  [{path, Path}, {distance, Distance},
    {stats, [
      {time, Time},
      {nodes, length(Path)},
      {visited_nodes, ets:info(VisitedNodes, size)},
      {memory, ets:info(VisitedNodes, memory) + ets:info(Tab, memory)}
    ]}
  ].
  
recurseNodes(Target, Target, _Queue, {Tab, _VisitedNodes}) ->
  readPath(Target, Tab);

recurseNodes(Node, Target, Queue, {Tab, VisitedNodes}) ->
  visited(Node, true, VisitedNodes),
  Q1 = priority_queue:remove({Node, add_heuristic(getDistance(Node, Tab), Node, Target)}, Queue),
  Neighbours = geodata:edges(Node),
  Q2 = updateDistances(Node, Target, Neighbours, {Tab, Q1, VisitedNodes}),
  case gb_trees:size(Q2) of
    0 -> Tab;
    _Any -> {ClosestNode, _Distance} = priority_queue:smallest(Q2),
      recurseNodes(ClosestNode, Target, Q2, {Tab, VisitedNodes})
  end.

updateDistance(Node, PreviousNode, OldDistance, NewDistance, Target, {Tab, Queue}) ->
  Q1 = priority_queue:remove({Node, add_heuristic(OldDistance, Node, Target)}, Queue),
  Q2 = priority_queue:add({Node, add_heuristic(NewDistance, Node, Target)}, Q1),
  ets:insert(Tab, {Node, {previous, PreviousNode}, {distance, NewDistance}}),
  Q2.

add_heuristic(undefined, _Node, _Target) ->
  undefined;

add_heuristic(Distance, Node, Target) ->
  Distance+geodata:distance(Node, Target).

updateDistances(CurrentNode, Target, Neighbours, {Tab, Queue, VisitedNodes}) ->
  CurrentDistance = getDistance(CurrentNode, Tab),
  recurseNeighbours(Neighbours, CurrentNode, CurrentDistance, Target, {Tab, Queue, VisitedNodes}).

recurseNeighbours([], _CurrentNode, _CurrentDistance, _Target, {_Tab, Queue, _VisitedNodes}) ->
  Queue;

recurseNeighbours([Neighbour|Rest], CurrentNode, CurrentDistance, Target, {Tab, Queue, VisitedNodes}) ->
  {{node, Node}, {distance, Distance}} = Neighbour,
  NewDistance = CurrentDistance + Distance,
  OldDistance = getDistance(Node, Tab),
  NotVisited = notVisited(Node, VisitedNodes),
  NewQueue = if (NewDistance < OldDistance) and NotVisited ->
    updateDistance(Node, CurrentNode, OldDistance, NewDistance, Target, {Tab, Queue});
    true -> Queue
  end,
  recurseNeighbours(Rest, CurrentNode, CurrentDistance, Target, {Tab, NewQueue, VisitedNodes}).

% helper functions
getDistance(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, _, {distance, Distance}}] -> Distance;
    [] -> undefined
  end.

% get previous node in path
getPrevious(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, {previous, Previous}, {distance, _Distance}}] -> Previous;
    [] -> undefined
  end.
  
% mark a node as visited
visited(Node, Bool, VisitedNodes) ->
  ets:insert(VisitedNodes, {Node, Bool}).
  
% check if node has not been visited
notVisited(Node, VisitedNodes) ->
  case ets:lookup(VisitedNodes, Node) of
    [{Node, true}] -> false;
    [] -> true
  end.
  
% go backwards through all nodes to read the path
readPath(Node, Tab) ->
  readPath(Node, [], Tab).
  
readPath(undefined, Path, _Tab) ->
  Path;  

readPath(Node, Path, Tab) ->
  readPath(getPrevious(Node, Tab), [Node|Path], Tab).