
% Store
%
% Author: Mirko Kiefer
%
% The interface to the ets tables storing node and way data. The ets tables should always be accessed through this module.

-module(store).
-export([node2wayids/1, lookup_way/1, lookup_node/1]).
-include("../includes/routing.hrl").

node2wayids(NodeID) ->
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