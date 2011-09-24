
% Database processing
%
% Author: Mirko Kiefer
%
% This module is called once on startup to load all database files and builds an additional database mapping nodes to ways. 

-module(processing).
-export([loadData/0]).


loadData() ->
  ets:file2tab("../output/osm_nodes.tab"),
  ets:file2tab("../output/osm_ways.tab"),
  ets:new(osm_nodes_to_ways, [named_table, bag, public]),
  processData(),
  ets:tab2file(osm_nodes_to_ways, "../output/osm_nodes_to_ways.tab").
  
processData() ->
  FirstKey = ets:first(osm_ways),
  [{ID, _, {refs, Refs}}] = ets:lookup(osm_ways, FirstKey),
  writeNodesToWay(Refs, ID),
  processData(ets:next(osm_ways, FirstKey)).

processData('$end_of_table') ->
  true;

processData(NextKey) ->
  [{ID, _, {refs, Refs}}] = ets:lookup(osm_ways, NextKey),
  writeNodesToWay(Refs, ID),
  %io:format("read way: ~p~n", [ID]),
  processData(ets:next(osm_ways, NextKey)).
  
writeNodesToWay(Refs, WayID) ->
  lists:foreach(fun(Ref) -> ets:insert(osm_nodes_to_ways, {Ref, WayID}) end, Refs).