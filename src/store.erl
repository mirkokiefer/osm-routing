
% Store
%
% Author: Mirko Kiefer
%
% The interface to the ets tables storing node and way data. The ets tables should always be accessed through this module.

-module(store).
-export([init/0, start/0, stop/0, serialize/0, node2wayids/1, lookup_way/1, lookup_node/1,
  store_way/1, store_node/1, store_node2wayid/2]).
-include("../includes/routing.hrl").

init() ->
  ets:new(osm_nodes, [named_table, set, public, {keypos, 2}]),
  ets:new(osm_ways, [named_table, set, public, {keypos, 2}]),
  ets:new(osm_nodes_to_ways, [named_table, bag, public]),
  ets:new(osm_names_to_nodes,[named_table, set, public]).

start() ->
  ets:file2tab(?NODES_DB),                %%speichere von der festplatte auf den arbeitsspeicher
  ets:file2tab(?WAYS_DB),                 %% wird über die file2tab funktion gemacht
  ets:file2tab(?NODES_TO_WAYS_DB).

stop() ->
  ets:delete(osm_nodes),
  ets:delete(osm_ways),
  ets:delete(osm_nodes_to_ways).

serialize() ->                             %% speichert tabellen auf die festplatte
  filelib:ensure_dir("../output/"),      
  ets:tab2file(osm_nodes, ?NODES_DB),      %%wird über tab2file funktion gemacht
  ets:tab2file(osm_ways, ?WAYS_DB),
  ets:tab2file(osm_nodes_to_ways, ?NODES_TO_WAYS_DB). 

node2wayids(NodeID) ->                                %%
  Result = ets:lookup(osm_nodes_to_ways, NodeID),
  [Way || {_Node, Way} <- Result].%%gibt wayid aus am ende(nicht way und node id)
  
lookup_way(WayID) ->                               %%lookup funktion gibt way aus
  case ets:lookup(osm_ways, WayID) of
    [] -> undefined;
    [Way|_] -> Way
  end.
  
lookup_node(NodeID) ->                            %%benutzen wir in name_server; erlaubt es über node Tabelle zu gucken
  case ets:lookup(osm_nodes, NodeID) of
    [] -> undefined;
    [Node|_] -> Node
  end.
  
store_way(Way) ->
  ets:insert(osm_ways, Way).
  
store_node(Node) ->
  ets:insert(osm_nodes, Node).
  
store_node2wayid(Node, WayID) ->
 ets:insert(osm_nodes_to_ways, {Node, WayID}).
