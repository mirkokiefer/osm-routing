
% Routing
%
% Author: Mirko Kiefer
%
% This module serves as the Erlang interface to the system.
% Other erlang software modules should always go through this interface when communicating with the routing system.

-module(routing).

-export([start/0, stop/0, load_osm_data/1]).

start() ->
  store:start(),
  server:start().
  %name_server:start().
  
stop() ->
  store:stop(),
  server:stop().
  %name_server:stop().
  
load_osm_data(File) ->
  osm_parser:read(File).
