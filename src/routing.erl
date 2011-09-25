
% Routing
%
% Author: Mirko Kiefer
%
% This module serves as the Erlang interface to the system.
% Other erlang software modules should always go through this interface when communicating with the routing system.

-module(routing).

-export([start/0, stop/0, load_osm_data/1]).

start() ->
  load_tables(),
  _Http = server:start().
  
stop() ->
  ets:delete(osm_nodes),
  ets:delete(osm_ways),
  ets:delete(osm_nodes_to_ways),
  server:stop().
  
load_osm_data(File) ->
  osm_parser:read(File).
  
load_tables() ->
  ets:file2tab("../output/osm_nodes.tab"),
  ets:file2tab("../output/osm_ways.tab"),
  ets:file2tab("../output/osm_nodes_to_ways.tab").