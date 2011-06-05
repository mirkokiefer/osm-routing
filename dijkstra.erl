-module(dijkstra).
-export([shortest_path/2]).

shortest_path(SourceID, TargetID) ->
  Tab = ets:new(shortest_path, [set, public]),
  VisitedNodes = ets:new(visited_nodes, [set, public]),
  Q1 = gb_trees:empty(),
  ets:insert(Tab, {SourceID, {previous, undefined}, {distance, 0}}),
  Path = recurseNodes(SourceID, TargetID, Q1, {Tab, VisitedNodes}),
  Distance = getDistance(TargetID, Tab),
  io:format("path: ~p~n distance: ~p nodes: ~p ", [Path, Distance, length(Path)]),
  io:format("visited nodes: ~p memory: ~p~n", [ets:info(VisitedNodes, size),
    ets:info(VisitedNodes, memory)+ets:info(Tab, memory)]),
  io:format("coordinates: ~p~n", [linkFromPath(Path)]).
  
recurseNodes(Target, Target, _Queue, {Tab, _VisitedNodes}) ->
  io:format("found target: ~p~n", [Target]),
  readPath(Target, Tab);

recurseNodes(Node, Target, Queue, {Tab, VisitedNodes}) ->
  visited(Node, true, VisitedNodes),
  Q1 = priority_queue:remove({Node, getDistance(Node, Tab)}, Queue),
  Neighbours = geodata:edges(Node),
  Q2 = updateDistances(Node, Target, Neighbours, {Tab, Q1, VisitedNodes}),
  case gb_trees:size(Q2) of
    0 -> Tab;
    _Any -> {ClosestNode, _Distance} = priority_queue:smallest(Q2),
      recurseNodes(ClosestNode, Target, Q2, {Tab, VisitedNodes})
  end.
  
readPath(Node, Tab) ->
  readPath(Node, [], Tab).
  
readPath(undefined, Path, _Tab) ->
  Path;  

readPath(Node, Path, Tab) ->
  readPath(getPrevious(Node, Tab), [Node|Path], Tab).

updateDistance(Node, PreviousNode, OldDistance, NewDistance, Target, {Tab, Queue}) ->
  Q1 = priority_queue:remove({Node, add_heuristic(OldDistance, Node, Target)}, Queue),
  Q2 = priority_queue:add({Node, add_heuristic(NewDistance, Node, Target)}, Q1),
  ets:insert(Tab, {Node, {previous, PreviousNode}, {distance, NewDistance}}),
  Q2.
  
add_heuristic(Distance, Node, Target) ->
  Distance.
  
getDistance(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, _, {distance, Distance}}] -> Distance;
    [] -> undefined
  end.
  
getPrevious(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, {previous, Previous}, {distance, _Distance}}] -> Previous;
    [] -> undefined
  end.
  
notVisited(Node, VisitedNodes) ->
  case ets:lookup(VisitedNodes, Node) of
    [{Node, true}] -> false;
    [] -> true
  end.
  
visited(Node, Bool, VisitedNodes) ->
  ets:insert(VisitedNodes, {Node, Bool}).

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
  
%utility functions
linkFromPath(Path) ->
  Coords = [geodata:geocoordinates(geodata:lookup_node(Node)) || Node <- Path],
  CoordsString = [string:join([float_to_string(Lat), ",", float_to_string(Lon)], "") || {Lat, Lon} <- Coords],
  Param = string:join(CoordsString, "|"),
  string:join(["http://maps.google.com/maps/api/staticmap?", "sensor=false",
    "&size=640x640", "&path=color:0x0000ff|weight:5|", Param], "").
  
float_to_string(Float) ->
  [String] = io_lib:format("~.7f",[Float]),
  String.