-module(geodata).
-export([route/2, edges/1, neighbours/2, distance/2, lookup_node/1, geocoordinates/1]).


route(Source, Target) ->
  undefined.
  
edges(Node) ->
  WayIDs = node2ways(Node),
  Ways = lists:map(fun({_, WayID}) -> WayLookup = lookup_way(WayID),
    [{_, _, {refs, WayDetails}}] = WayLookup, WayDetails end, WayIDs),
  Neighbours = lists:flatten(lists:map(fun(Refs) -> neighbours(Node, Refs) end, Ways)),
  Neighbours.
  
neighbours(Element, List) ->
  case List of
    [] -> Result = [];
    [Element] -> Result = [];
    [Element | Rest] -> [Next | _] = Rest, Result = [Next];
    [First | Rest] -> Result = neighbours(Element, First, Rest)
  end,
  Result.
  
neighbours(Element, Last, List) ->
  case List of
    [] -> Result = [Last];
    [Element | []] -> Result = [Last];
    [Element | Rest] -> [Next | _] = Rest, Result = [Last, Next];
    [First | []] -> Result = [];
    [First | Rest] -> Result = neighbours(Element, First, Rest)
  end,
  Result.

distance(NodeAID, NodeBID) ->
  {ALat, ALon} = geocoordinates(lookup_node(NodeAID)),
  {BLat, BLon} = geocoordinates(lookup_node(NodeBID)),
  LatDiff = deg2rad(ALat-BLat),
  LonDiff = deg2rad(ALon-BLon),
  R = 6371,
  A = math:sin(LatDiff/2) * math:sin(LatDiff/2) +
    math:cos(deg2rad(ALat)) * math:cos(deg2rad(BLat)) *
    math:sin(LonDiff/2) * math:sin(LonDiff/2),
  C = 2*math:atan2(math:sqrt(A), math:sqrt(1-A)),
  R*C.


% ets accessing functions:
node2ways(NodeID) ->
  ets:lookup(osm_nodes_to_ways, NodeID).
  
lookup_way(WayID) ->
  [Way] = ets:lookup(osm_ways, WayID),
  Way.
  
lookup_node(NodeID) ->
  [Node] = ets:lookup(osm_nodes, NodeID),
  Node.
  
% operators on ets data:
geocoordinates(Node) ->
  {_, {lat, LatString}, {lon, LonString}, _} = Node,
  {LatFloat, _} = string:to_float(LatString),
  {LonFloat, _} = string:to_float(LonString),
  {LatFloat, LonFloat}.
  
deg2rad(Deg) -> Deg*math:pi()/180.