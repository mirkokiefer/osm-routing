-module(name_server).
-export([show_all_nodes/0]).


show_all_nodes() ->
  First = ets:first(osm_nodes),
  show_all_nodes(First).

show_all_nodes(Previous) ->
  Next = ets:next(osm_nodes, Previous),
  
  case Next of
    '$end_of_table' -> stopp;
    Any -> 
      log_node(Next),
      show_all_nodes(Next) 
  end.
  
log(Element) ->
  io:format("Log: ~p~n", [Element]).

log_node(NodeID) ->
  Node = store:lookup_node(NodeID),
  {node,ID,Lat,Lon,Attributes} = Node,
  case show_name(Attributes) of
    [] -> ignore;
    [Any] -> log(Any)
  end.

show_name(Attributes) ->
  [Value||{Key,Value}<-Attributes, Key == "name"].
