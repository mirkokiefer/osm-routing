-module(geodata).
-export([route/2, edges/1, neighbours/2]).


route(Source, Target) ->
  undefined.
  
edges(Node) ->
  WayIDs = ets:lookup(osm_nodes_to_ways, Node),
  Ways = lists:map(fun({_, WayID}) -> WayLookup = ets:lookup(osm_ways, WayID),
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
