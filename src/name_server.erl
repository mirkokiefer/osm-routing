-module(name_server).
-export([show_all_nodes/0]).


show_all_nodes() ->
  First = ets:first(osm_nodes),
  show_all_nodes(First).

show_all_nodes(Previous) ->
  Next = ets:next(osm_nodes, Previous),
  log(Next),
  case Next of
    '$end_of_table' -> stopp;
    Any -> show_all_nodes(Next) 
  end.
  
log(Element) ->
  io:format("Log: ~p~n", [Element]).

