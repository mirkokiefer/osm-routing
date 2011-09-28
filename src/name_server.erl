-module(name_server).
-export([show_all_nodes/0]).


show_all_nodes() ->
  ets:new(osm_names_to_nodes,[named_table, set, public]),    %%Tabelle wird angelegt
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
    [Any] -> store_name(Any,ID)           %%log ist rausgemacht, anstattdessen store_name hinzugefügt (aus store)
  end.
  
show_name(Attributes) ->
  [Value||{Key,Value}<-Attributes, Key == "name"].    

store_name(Name,ID) ->                                %% aus store module
  ets:insert(osm_names_to_nodes, {Name,ID}).

lookup_name(Name) ->
 Result = ets:lookup(osm_names_to_nodes, Name),
 Result.

