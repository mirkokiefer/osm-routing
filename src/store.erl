-module(store).
-export([node2ways/1, lookup_way/1, lookup_node/1]).
-include("../includes/routing.hrl").

node2ways(NodeID) ->
  Result = ets:lookup(osm_nodes_to_ways, NodeID),
  [Way || {_Node, Way} <- Result].
  
lookup_way(WayID) ->
  case ets:lookup(osm_ways, WayID) of
    [{Id, {tags, Tags}, {refs, Refs}}] -> #way{id=Id, tags=Tags, refs=Refs};
    _ -> undefined
  end.
  
lookup_node(NodeID) ->
  case ets:lookup(osm_nodes, NodeID) of
    [{Id, {lat, Lat}, {lon ,Lon}, {tags, Tags}}] -> #node{id=Id, lat=Lat, lon=Lon, tags=Tags};
    [] -> undefined
  end.