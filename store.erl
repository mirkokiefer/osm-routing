-module(store).
-export([node2ways/1, lookup_way/1, lookup_node/1]).

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