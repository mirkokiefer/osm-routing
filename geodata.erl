-module(geodata).
-export([route/2, route_simple/2, route_annotated/2, route_description/2, route_description_data/2, edges/1, distance/2, nodes_to_coords/1, node2ways/1, lookup_way/1, lookup_node/1, coordinates/1]).

-export([extract_way_tag/2]).

route(SourceID, TargetID) ->
  astar:shortest_path(SourceID, TargetID).
  
route_simple(SourceID, TargetID) ->
  [{path, Nodes}, _, _] = route(SourceID, TargetID),
  Nodes.
  
route_with_distances(SourceID, TargetID) ->
  astar:shortest_path_with_distances(SourceID, TargetID).
  
route_annotated(SourceID, TargetID) ->
  [{path, Nodes}, D, S] = route_with_distances(SourceID, TargetID),
  [{path, group_nodes(Nodes)}, D, S].
  
route_description(SourceID, TargetID) ->
  [{path, Path}, D] = route_description_data(SourceID, TargetID),
  Description = route_description_internal(Path, []).
  
route_description_internal([], Output) ->
  lists:reverse(Output);
  
route_description_internal([[{way, Way}, {distance, Distance}, {angle, Angle}]|Rest], Output) ->
  Direction = angle_to_direction(Angle),
  NewOutput = [[{way, Way}, {distance, Distance}, {direction, Direction}]|Output],
  route_description_internal(Rest, NewOutput).
  
route_description_data(SourceID, TargetID) ->
  [{path, Path}, D, _S] = route_annotated(SourceID, TargetID),
  NewPath = calculate_way_distances(Path, []),
  [{path, NewPath}, D].

calculate_way_distances([[{way, Way}, {nodes, Nodes}, {angle, Angle}]], Output) ->
  NewOutput = case Nodes of
    [_SingleNode] -> [[{way, Way}, {distance, 0}, {angle, Angle}]|Output];
    [[_, {distance, FirstD}]|Rest] -> [_, {distance, LastD}] = lists:last(Rest),
      [[{way, Way}, {distance, LastD-FirstD}, {angle, Angle}]|Output]
  end,
  lists:reverse(NewOutput);

calculate_way_distances([[{way, Way}, {nodes, Nodes}, {angle, Angle}]|Rest], Output) ->
  [[_, {nodes, NextNodes}, _]|_]=Rest,
  [[_, {distance, FirstD}]|_] = Nodes,
  [[_, {distance, NextFirstD}]|_] = NextNodes,
  Distance = NextFirstD-FirstD,
  NewOutput = [[{way, Way}, {distance, Distance}, {angle, Angle}]|Output],
  calculate_way_distances(Rest, NewOutput).
  
edges(NodeID) ->
  WayIds = node2ways(NodeID),
  Ways = lists:map(fun(WayID) -> WayLookup = lookup_way(WayID),
    {_, _, {refs, WayDetails}} = WayLookup, WayDetails end, WayIds),
  Neighbours = lists:flatten(lists:map(fun(Refs) -> neighbours(NodeID, Refs) end, Ways)),
  NeighboursDetails = [{{node, Neighbour}, {distance, distance(NodeID, Neighbour)}} ||
    Neighbour <- Neighbours, lookup_node(Neighbour) =/= undefined],
  NeighboursDetails.

distance(NodeAID, NodeBID) ->
  {ALat, ALon} = coordinates(lookup_node(NodeAID)),
  {BLat, BLon} = coordinates(lookup_node(NodeBID)),
  LatDiff = deg2rad(ALat-BLat),
  LonDiff = deg2rad(ALon-BLon),
  R = 6367500,
  A = math:sin(LatDiff/2) * math:sin(LatDiff/2) +
    math:cos(deg2rad(ALat)) * math:cos(deg2rad(BLat)) *
    math:sin(LonDiff/2) * math:sin(LonDiff/2),
  C = 2*math:atan2(math:sqrt(A), math:sqrt(1-A)),
  R*C.

nodes_to_coords(List) ->
  [geodata:coordinates(geodata:lookup_node(Node)) || Node <- List].
  
group_nodes(Nodes) ->
  compute_angles(group_nodes(Nodes, [{way, start}, {nodes, []}], [])).
  
group_nodes([First], [{way, Way}, {nodes, Nodes}], List) ->
  NewGroup = [{way, Way}, {nodes, lists:reverse([First|Nodes])}],
  [_Undefined | Rest] = lists:reverse([NewGroup|List]),
  Rest;

group_nodes([First|Rest], [{way, WayName}, {nodes, Nodes}], List) ->
  [Second|_] = Rest,
  [{node, FirstNode}, _] = First,
  [{node, SecondNode}, _] = Second,
  NewWay = connecting_way(FirstNode, SecondNode),
  NewWayName = geodata:extract_way_tag("name", NewWay),
  case NewWayName of
    WayName -> NewGroup = [{way, WayName}, {nodes, [First|Nodes]}], NewList = List;
    _ -> NewGroup = [{way, NewWayName}, {nodes, [First]}],
      NewList = [[{way, WayName},{nodes, lists:reverse(Nodes)}] | List]
  end,
  group_nodes(Rest, NewGroup, NewList).
  
compute_angles(GroupedNodes) ->
  compute_angles(GroupedNodes, []).

compute_angles([_First], List) ->
  lists:reverse(List);
  
compute_angles([[_, {nodes, FirstNodes}]|Rest], List) ->
  A = lists:last(FirstNodes),
  [[{way, SecondWay}, {nodes, SecondNodes}]|NewRest] = Rest,
  case SecondNodes of
    [B,C|_] -> ok;
    [B] -> [[_, {nodes, [C|_]}] | _]=NewRest
  end,
  [{node, NodeA}, _] = A,
  [{node, NodeB}, _] = B,
  [{node, NodeC}, _] = C,
  Angle = angle(NodeA, NodeB, NodeC),
  NewList = [[{way, SecondWay}, {nodes, SecondNodes}, {angle, Angle}]|List],
  compute_angles(Rest, NewList).

bearing(NodeAID, NodeBID) ->
  {ALatDeg, ALonDeg} = coordinates(lookup_node(NodeAID)),
  {BLatDeg, BLonDeg} = coordinates(lookup_node(NodeBID)),
  ALat = deg2rad(ALatDeg),
  ALon = deg2rad(ALonDeg),
  BLat = deg2rad(BLatDeg),
  BLon = deg2rad(BLonDeg),
  DLon = ALon-BLon,
  Y = math:sin(DLon)*math:cos(BLat),
  X = math:cos(ALat)*math:sin(BLat)-math:sin(ALat)*math:cos(BLat)*math:cos(DLon),
  rad2deg(math:atan2(Y, X))*(-1).
  
angle(A, B, C) ->
  Angle = bearing(A, B)-bearing(B, C),
  NormalizedAngle = case abs(Angle)>180 of
    true -> (-1*Angle/abs(Angle)) * (180- (abs(Angle)-180));
    false -> Angle
  end,
  NormalizedAngle.

% ets accessing functions:
node2ways(NodeID) ->
  Result = ets:lookup(osm_nodes_to_ways, NodeID),
  [Way || {_Node, Way} <- Result].
  
lookup_way(WayID) ->
  case ets:lookup(osm_ways, WayID) of
    [Way] -> Way;
    _ -> undefined
  end.
  
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

extract_way_tag(FilterTag, WayID) ->
  case lookup_way(WayID) of
    {_, {tags, Tags}, _} ->
      case [Value || {Tag, Value} <- Tags, Tag == FilterTag] of
        [First|_] -> First;
        [] -> undefined
      end;
    undefined -> undefined
  end.

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
  
connecting_way(NodeA, NodeB) ->
  WaysA = node2ways(NodeA),
  WaysB = node2ways(NodeB),
  case intersection(WaysA, WaysB) of
    [] -> undefined;
    [Way] -> Way;
    [First|_] -> First
  end.

deg2rad(Deg) -> Deg*math:pi()/180.

rad2deg(Rad) -> Rad*180/math:pi().

float_to_string(Float) ->
  [String] = io_lib:format("~.7f",[Float]),
  String.
   
direction(Direction) ->
  case Direction of
    straight -> "gehen Sie geradeaus";
    left -> "biegen Sie nach links ab";
    right -> "biegen Sie nach rechts ab";
    to -> "in"
  end.
  
angle_to_direction(Angle) ->
  if
    Angle > 30 -> direction(left);
    Angle < -30 -> direction(right);
    true -> direction(straight)
  end.
    

%utility functions
linkFromPath(Path) ->
  Coords = [geodata:coordinates(geodata:lookup_node(Node)) || Node <- Path],
  CoordsString = [string:join([float_to_string(Lat), ",", float_to_string(Lon)], "") || {Lat, Lon} <- Coords],
  Param = string:join(CoordsString, "|"),
  string:join(["http://maps.google.com/maps/api/staticmap?", "sensor=false",
    "&size=640x640", "&path=color:0x0000ff|weight:5|", Param], "").
  
intersection(L1,L2) -> lists:filter(fun(X) -> lists:member(X,L1) end, L2).