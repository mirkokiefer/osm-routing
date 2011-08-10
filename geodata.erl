-module(geodata).
-export([way_distances/1, group_nodes/1, edges/1, distance/2, nodes_to_coords/1, coordinates/1]).

-export([extract_way_tag/2]).

way_distances(GroupedNodes) ->
  calculate_way_distances(GroupedNodes, []).

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
  WayIds = store:node2ways(NodeID),
  Ways = lists:map(fun(WayID) -> WayLookup = store:lookup_way(WayID),
    {_, _, {refs, WayDetails}} = WayLookup, WayDetails end, WayIds),
  Neighbours = lists:flatten(lists:map(fun(Refs) -> neighbours(NodeID, Refs) end, Ways)),
  NeighboursDetails = [{{node, Neighbour}, {distance, distance(NodeID, Neighbour)}} ||
    Neighbour <- Neighbours, store:lookup_node(Neighbour) =/= undefined],
  NeighboursDetails.

distance(NodeAID, NodeBID) ->
  {ALat, ALon} = coordinates(store:lookup_node(NodeAID)),
  {BLat, BLon} = coordinates(store:lookup_node(NodeBID)),
  LatDiff = utils:deg2rad(ALat-BLat),
  LonDiff = utils:deg2rad(ALon-BLon),
  R = 6367500,
  A = math:sin(LatDiff/2) * math:sin(LatDiff/2) +
    math:cos(utils:deg2rad(ALat)) * math:cos(utils:deg2rad(BLat)) *
    math:sin(LonDiff/2) * math:sin(LonDiff/2),
  C = 2*math:atan2(math:sqrt(A), math:sqrt(1-A)),
  R*C.

nodes_to_coords(List) ->
  [geodata:coordinates(store:lookup_node(Node)) || Node <- List].
  
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
  {ALatDeg, ALonDeg} = coordinates(store:lookup_node(NodeAID)),
  {BLatDeg, BLonDeg} = coordinates(store:lookup_node(NodeBID)),
  ALat = utils:deg2rad(ALatDeg),
  ALon = utils:deg2rad(ALonDeg),
  BLat = utils:deg2rad(BLatDeg),
  BLon = utils:deg2rad(BLonDeg),
  DLon = ALon-BLon,
  Y = math:sin(DLon)*math:cos(BLat),
  X = math:cos(ALat)*math:sin(BLat)-math:sin(ALat)*math:cos(BLat)*math:cos(DLon),
  utils:rad2deg(math:atan2(Y, X))*(-1).
  
angle(A, B, C) ->
  Angle = bearing(A, B)-bearing(B, C),
  NormalizedAngle = case abs(Angle)>180 of
    true -> (-1*Angle/abs(Angle)) * (180- (abs(Angle)-180));
    false -> Angle
  end,
  NormalizedAngle.
  
% operators on ets data:
coordinates(Node) ->
  {_, {lat, LatString}, {lon, LonString}, _} = Node,
  {LatFloat, _} = string:to_float(LatString),
  {LonFloat, _} = string:to_float(LonString),
  {LatFloat, LonFloat}.

extract_way_tag(FilterTag, WayID) ->
  case store:lookup_way(WayID) of
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
  WaysA = store:node2ways(NodeA),
  WaysB = store:node2ways(NodeB),
  case utils:intersection(WaysA, WaysB) of
    [] -> undefined;
    [Way] -> Way;
    [First|_] -> First
  end.