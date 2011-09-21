-module(routing).

-export([start/0, stop/0, load_osm_data/1]).

start() ->
  processing:loadData(),
  _Http = server:start().
  
stop() ->
  ets:delete(osm_nodes),
  ets:delete(osm_ways),
  ets:delete(osm_nodes_to_ways),
  server:stop().
  
load_osm_data(File) ->
  osm_parser:read(File).