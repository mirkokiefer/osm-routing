
% Store interface
%
% Author: Mirko Kiefer
%
% This module provides an intermediate interface between the store module and the rest of the system.
% Any high-level request required in the system should be implemented here.

-module(geodata).
-export([neighbours/1, distance/2, nodeid_to_coords/1, nodes_to_coords/1, path_angles/1, connecting_way/2, way_tag/2]).

-include("../includes/routing.hrl").

neighbours(NodeID) ->
  WayIds = store:node2wayids(NodeID),
  Refs = lists:map(fun(WayID) ->
    #way{refs=Refs} = store:lookup_way(WayID),
    Refs
  end, WayIds),
  Neighbours = utils:flatten_once(Refs),
  NeighboursDetails = [{{node, Neighbour}, {distance, distance(NodeID, Neighbour)}} ||
    Neighbour <- Neighbours, store:lookup_node(Neighbour) =/= undefined],
  NeighboursDetails.

distance(NodeAID, NodeBID) ->
  {ALat, ALon} = coords(store:lookup_node(NodeAID)),
  {BLat, BLon} = coords(store:lookup_node(NodeBID)),
  LatDiff = utils:deg2rad(ALat-BLat),
  LonDiff = utils:deg2rad(ALon-BLon),
  R = 6367500,
  A = math:sin(LatDiff/2) * math:sin(LatDiff/2) +
    math:cos(utils:deg2rad(ALat)) * math:cos(utils:deg2rad(BLat)) *
    math:sin(LonDiff/2) * math:sin(LonDiff/2),
  C = 2*math:atan2(math:sqrt(A), math:sqrt(1-A)),
  R*C.
  
path_angles(Path) ->
  Angles = lists:reverse(path_angles_recursive(Path, [])),
  [First|_] = Path,
  [lists:append(First, [{angle, 0}]) | Angles].

path_angles_recursive([_SecondLast, Last], List) ->
  [lists:append(Last, [{angle, 0}]) | List];

path_angles_recursive([Previous, Current, Next | Rest], List) ->
  [PreviousID, CurrentID, NextID] = [ID || [{node, ID}, _] <- [Previous, Current, Next]],
  NewCurrent = lists:append(Current, [{angle, angle(PreviousID, CurrentID, NextID)}]),
  NewList = [NewCurrent | List],
  path_angles_recursive([Current, Next | Rest], NewList).
  
connecting_way(NodeA, NodeB) ->
  WaysA = store:node2wayids(NodeA),
  WaysB = store:node2wayids(NodeB),
  case utils:intersection(WaysA, WaysB) of
    [] -> undefined;
    [Way] -> Way;
    [First|_] -> First
  end.
  
way_tag(FilterTag, WayID) ->
  case store:lookup_way(WayID) of
    undefined -> undefined;
    #way{tags=Tags} ->
      case [Value || {Tag, Value} <- Tags, Tag == FilterTag] of
        [First|_] -> First;
        [] -> undefined
      end
  end.
  
nodeid_to_coords(NodeID) ->
  coords(store:lookup_node(NodeID)).

nodes_to_coords(List) ->
  [coords(store:lookup_node(Node)) || Node <- List].

bearing(NodeAID, NodeBID) ->
  {ALatDeg, ALonDeg} = coords(store:lookup_node(NodeAID)),
  {BLatDeg, BLonDeg} = coords(store:lookup_node(NodeBID)),
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

% helper functions
coords(Node) ->
  {LatFloat, _} = string:to_float(Node#node.lat),
  {LonFloat, _} = string:to_float(Node#node.lon),
  {LatFloat, LonFloat}.