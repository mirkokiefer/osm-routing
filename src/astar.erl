-module(astar).
-export([shortest_path/2, shortest_path_with_distances/2]).

-include("../includes/routing.hrl").

-record(state, {tab, queue, visited_nodes}).

shortest_path(SourceID, TargetID) ->
  [{route, Route}, _Tab] = shortest_path_internal(SourceID, TargetID),
  Route.

shortest_path_with_distances(SourceID, TargetID) ->
  [{route, Route=#route{path=Path}}, {tab, Tab}] = shortest_path_internal(SourceID, TargetID),
  NewPath = [[{node, Node}, {distance, getDistance(Node, Tab)}] || Node <- Path],
  Route#route{path=NewPath}.

shortest_path_internal(SourceID, TargetID) ->
  StartT = now(),
  Tab = ets:new(shortest_path, [set, public]),
  VisitedNodes = ets:new(visited_nodes, [set, public]),
  Q1 = priority_queue:new(),
  
  ets:insert(Tab, {SourceID, {previous, undefined}, {distance, 0}}),
  Path = recurseNodes(SourceID, TargetID, #state{tab=Tab, queue=Q1, visited_nodes=VisitedNodes}),
  Distance = getDistance(TargetID, Tab),
  EndT = now(),
  Time = timer:now_diff(EndT, StartT),
  Route = #route{
    path=Path,
    distance=Distance,
    time=Time,
    nodes=length(Path),
    visited_nodes=ets:info(VisitedNodes, size),
    memory=ets:info(VisitedNodes, memory) + ets:info(Tab, memory)
  },
  [{route,Route}, {tab, Tab}].

% recursively go through the respectively closest node in the priority queue to find target
recurseNodes(Target, Target, State) ->
  readPath(Target, State#state.tab);

recurseNodes(Node, Target, State=#state{tab=Tab, queue=Queue, visited_nodes=Visited}) ->
  visited(Node, true, Visited),
  OldQueueEntry = add_heuristic(getDistance(Node, Tab), Node, Target),
  Q1 = priority_queue:remove({Node, OldQueueEntry}, Queue),
  Neighbours = geodata:neighbours(Node),
  Q2 = updateDistances(Node, Target, Neighbours, State#state{queue=Q1}),
  case gb_trees:size(Q2) of
    0 -> Tab;
    _Any -> {ClosestNode, _Distance} = priority_queue:smallest(Q2),
      recurseNodes(ClosestNode, Target, State#state{queue=Q2})
  end.

% go through all neighbours and update distances
updateDistances(CurrentNode, Target, Neighbours, State) ->
  CurrentDistance = getDistance(CurrentNode, State#state.tab),
  recurseNeighbours(Neighbours, CurrentNode, CurrentDistance, Target, State).

recurseNeighbours([], _CurrentNode, _CurrentDistance, _Target, State) ->
  State#state.queue;

recurseNeighbours([Neighbour|Rest], CurrentNode, CurrentDistance, Target, State) ->
  {{node, Node}, {distance, Distance}} = Neighbour,
  NewDistance = CurrentDistance + Distance,
  OldDistance = getDistance(Node, State#state.tab),
  NotVisited = notVisited(Node, State#state.visited_nodes),
  NewQueue = if (NewDistance < OldDistance) and NotVisited ->
    updateDistance(Node, CurrentNode, OldDistance, NewDistance, Target, State);
    true -> State#state.queue
  end,
  recurseNeighbours(Rest, CurrentNode, CurrentDistance, Target, State#state{queue=NewQueue}).

% returns an updated version of the priority queue
updateDistance(Node, PreviousNode, OldDistance, NewDistance, Target, State) ->
  Q1 = priority_queue:remove({Node, add_heuristic(OldDistance, Node, Target)}, State#state.queue),
  Q2 = priority_queue:add({Node, add_heuristic(NewDistance, Node, Target)}, Q1),
  ets:insert(State#state.tab, {Node, {previous, PreviousNode}, {distance, NewDistance}}),
  Q2.

% add A* heuristic using distance to target approximation
add_heuristic(undefined, _Node, _Target) ->
  undefined;

add_heuristic(Distance, Node, Target) ->
  Distance+geodata:distance(Node, Target).

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