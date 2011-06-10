-module(geodata).
-export([route/2, edges/1, nodes_to_coords/1, neighbours/2, distance/2, lookup_node/1, coordinates/1]).


route(SourceID, TargetID) ->
  dijkstra:shortest_path(SourceID, TargetID).
  
edges(NodeID) ->
  WayIDs = node2ways(NodeID),
  Ways = lists:map(fun({_, WayID}) -> WayLookup = lookup_way(WayID),
    {_, _, {refs, WayDetails}} = WayLookup, WayDetails end, WayIDs),
  Neighbours = lists:flatten(lists:map(fun(Refs) -> neighbours(NodeID, Refs) end, Ways)),
  NeighboursDetails = [{{node, Neighbour}, {distance, distance(NodeID, Neighbour)}} ||
    Neighbour <- Neighbours, lookup_node(Neighbour) =/= undefined],
  NeighboursDetails.

distance(NodeAID, NodeBID) ->
  {ALat, ALon} = coordinates(lookup_node(NodeAID)),
  {BLat, BLon} = coordinates(lookup_node(NodeBID)),
  LatDiff = deg2rad(ALat-BLat),
  LonDiff = deg2rad(ALon-BLon),
  R = 6371000,
  A = math:sin(LatDiff/2) * math:sin(LatDiff/2) +
    math:cos(deg2rad(ALat)) * math:cos(deg2rad(BLat)) *
    math:sin(LonDiff/2) * math:sin(LonDiff/2),
  C = 2*math:atan2(math:sqrt(A), math:sqrt(1-A)),
  R*C.

nodes_to_coords(List) ->
  [geodata:coordinates(geodata:lookup_node(Node)) || Node <- List].

% ets accessing functions:
node2ways(NodeID) ->
  ets:lookup(osm_nodes_to_ways, NodeID).
  
lookup_way(WayID) ->
  [Way] = ets:lookup(osm_ways, WayID),
  Way.
  
lookup_node(NodeID) ->
  case ets:lookup(osm_nodes, NodeID) of
    [Node] -> Node;
    [] -> undefined
  end.
  
% operators on ets data:
coordinates(Node) ->
  {_, {lat, LatString}, {lon, LonString}, _} = Node,
  {LatFloat, _} = string:to_float(LatString),
  {LonFloat, _} = string:to_float(LonString),
  {LatFloat, LonFloat}.

% helper functions
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
    [_First | []] -> Result = [];
    [First | Rest] -> Result = neighbours(Element, First, Rest)
  end,
  Result.

deg2rad(Deg) -> Deg*math:pi()/180.

float_to_string(Float) ->
  [String] = io_lib:format("~.7f",[Float]),
  String.

%utility functions
linkFromPath(Path) ->
  Coords = [geodata:coordinates(geodata:lookup_node(Node)) || Node <- Path],
  CoordsString = [string:join([float_to_string(Lat), ",", float_to_string(Lon)], "") || {Lat, Lon} <- Coords],
  Param = string:join(CoordsString, "|"),
  string:join(["http://maps.google.com/maps/api/staticmap?", "sensor=false",
    "&size=640x640", "&path=color:0x0000ff|weight:5|", Param], "").
  