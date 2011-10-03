
% Store
%
% Author: Mirko Kiefer
%
% The interface to the ets tables storing node and way data. The ets tables should always be accessed through this module.

-module(store).
-export([init/0, start/0, stop/0, serialize/0, node2wayids/1, lookup_way/1, lookup_node/1,
  store_way/1, store_node/1]).
-include("../includes/routing.hrl").

init() ->
  start().

start() ->
  {ok, C} = eredis:start_link(),
  register(routing, C).

stop() ->
  stopped.

serialize() ->
  filelib:ensure_dir("../output/"),
  serialized. 

node2wayids(NodeID) ->
  case eredis:q(routing, ["HGET", node2ways, NodeID]) of
    {ok,undefined} -> [];
    {ok, Ways} -> binary_to_term(Ways)
  end.
  
lookup_way(WayID) ->
  case eredis:q(routing, ["HGET", ways, WayID]) of
    {ok,undefined} -> undefined;
    {ok, Way} -> binary_to_term(Way)
  end.
  
lookup_node(NodeID) ->
  case eredis:q(routing, ["HGET", nodes, NodeID]) of
    {ok,undefined} -> undefined;
    {ok, Node} -> binary_to_term(Node)
  end.
  
store_way(Way=#way{id=ID, refs=Refs}) ->
  store_nodeids2wayid(Refs, ID),
  eredis:q(routing, ["HSET", ways, ID, term_to_binary(Way)]).
  
store_node(Node=#node{id=ID}) ->
  eredis:q(routing, ["HSET", nodes, ID, term_to_binary(Node)]).

store_nodeids2wayid([], _WayID) ->
  success;

store_nodeids2wayid([FirstNodeID|Rest], WayID) ->
  store_nodeid2wayid(FirstNodeID, WayID),
  store_nodeids2wayid(Rest, WayID).
  
store_nodeid2wayid(NodeID, WayID) ->
  eredis:q(routing, ["HSET", node2ways, NodeID, term_to_binary([WayID|node2wayids(NodeID)])]).