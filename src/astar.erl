
% A*-Algorithm
%
% Author: Mirko Kiefer, Haykuhi Jaghinyan
%
% This module implements A* to find the shortest path between two nodes
% It makes use of the geodata module for accessing data.
% The priority queue used by the algorithm is implemented in the priority_queue module.

-module(astar).
-export([shortest_path/2, shortest_path_with_distances/2, analyse/0]).

-include("../includes/routing.hrl").

-record(state, {tab, queue, visited_nodes}).

analyse() ->
  fprof:profile(file, "astar.trace"),
  fprof:analyse([{dest, "astar.analysis"}, {cols, 120}]).

shortest_path(SourceID, TargetID) ->
  fprof:trace(start, "astar.trace"),
  [{route, Route}, _Tab] = shortest_path_internal(SourceID, TargetID),
  fprof:trace(stop),
  Route.

shortest_path_with_distances(SourceID, TargetID) ->
  [{route, Route=#route{path=Path}}, {tab, Tab}] = shortest_path_internal(SourceID, TargetID),
  NewPath = [[{node, Node}, {distance, get_distance(Node, Tab)}] || Node <- Path],
  Route#route{path=NewPath}.

shortest_path_internal(SourceID, TargetID) ->
  StartT = now(),
  Tab = ets:new(shortest_path, [set, public]),
  VisitedNodes = ets:new(visited_nodes, [set, public]),
  Q1 = priority_queue:new(),
  
  ets:insert(Tab, {SourceID, {previous, undefined}, {distance, 0}}),
  Path = recurse_nodes(SourceID, TargetID, #state{tab=Tab, queue=Q1, visited_nodes=VisitedNodes}),
  Distance = get_distance(TargetID, Tab),
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
recurse_nodes(Target, Target, State) ->
  read_path(Target, State#state.tab);

recurse_nodes(Node, Target, State=#state{tab=Tab, queue=Queue, visited_nodes=Visited}) ->
  visited(Node, true, Visited),
  OldQueueEntry = add_heuristic(get_distance(Node, Tab), Node, Target),
  Q1 = priority_queue:remove({Node, OldQueueEntry}, Queue),
  Neighbours = neighbours(Node),
  Q2 = update_distances(Node, Target, Neighbours, State#state{queue=Q1}),
  case gb_trees:size(Q2) of
    0 -> Tab;
    _Any -> {ClosestNode, _Distance} = priority_queue:smallest(Q2),
      recurse_nodes(ClosestNode, Target, State#state{queue=Q2})
  end.

% go through all neighbours and update distances
update_distances(CurrentNode, Target, Neighbours, State) ->
  CurrentDistance = get_distance(CurrentNode, State#state.tab),
  recurse_neighbours(Neighbours, CurrentNode, CurrentDistance, Target, State).

recurse_neighbours([], _CurrentNode, _CurrentDistance, _Target, State) ->
  State#state.queue;

recurse_neighbours([Neighbour|Rest], CurrentNode, CurrentDistance, Target, State) ->
  {{node, Node}, {distance, Distance}} = Neighbour,
  NewDistance = CurrentDistance + Distance,
  OldDistance = get_distance(Node, State#state.tab),
  NotVisited = not_visited(Node, State#state.visited_nodes),
  NewQueue = if (NewDistance < OldDistance) and NotVisited ->
    update_distance(Node, CurrentNode, OldDistance, NewDistance, Target, State);
    true -> State#state.queue
  end,
  recurse_neighbours(Rest, CurrentNode, CurrentDistance, Target, State#state{queue=NewQueue}).

% returns an updated version of the priority queue
update_distance(Node, PreviousNode, OldDistance, NewDistance, Target, State) ->
  Q1 = priority_queue:remove({Node, add_heuristic(OldDistance, Node, Target)}, State#state.queue),
  Q2 = priority_queue:add({Node, add_heuristic(NewDistance, Node, Target)}, Q1),
  ets:insert(State#state.tab, {Node, {previous, PreviousNode}, {distance, NewDistance}}),
  Q2.

% add A* heuristic using distance to target approximation
add_heuristic(undefined, _Node, _Target) ->
  undefined;

add_heuristic(Distance, Node, Target) ->
  Distance+distance(Node, Target).

% helper functions
get_distance(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, _, {distance, Distance}}] -> Distance;
    [] -> undefined
  end.

% get previous node in path
get_previous(Node, Tab) ->
  case ets:lookup(Tab, Node) of
    [{_Node, {previous, Previous}, {distance, _Distance}}] -> Previous;
    [] -> undefined
  end.
  
% mark a node as visited
visited(Node, Bool, VisitedNodes) ->
  ets:insert(VisitedNodes, {Node, Bool}).
  
% check if node has not been visited
not_visited(Node, VisitedNodes) ->
  case ets:lookup(VisitedNodes, Node) of
    [{Node, true}] -> false;
    [] -> true
  end.
  
% go backwards through all nodes to read the path
read_path(Node, Tab) ->
  read_path(Node, [], Tab).
  
read_path(undefined, Path, _Tab) ->
  Path;  

read_path(Node, Path, Tab) ->
  read_path(get_previous(Node, Tab), [Node|Path], Tab).
  
% data accessing functions
distance(NodeA, NodeB) -> geodata:distance(NodeA, NodeB).

neighbours(Node) -> geodata:neighbours(Node).