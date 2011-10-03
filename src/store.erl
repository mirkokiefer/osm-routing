
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
  merle:connect().

start() ->
  merle:connect().

stop() ->
  %mcd:do(routing, flush_all),
  stopped.

serialize() ->
  filelib:ensure_dir("../output/"),
  serialized. 

node2wayids(NodeID) ->
  case merle:getkey(node2way_key(NodeID)) of
    undefined -> [];
    Ways -> Ways
  end.
  
lookup_way(WayID) ->
  case merle:getkey(way_key(WayID)) of
    undefined -> undefined;
    Way -> Way
  end.
  
lookup_node(NodeID) ->
  case merle:getkey(node_key(NodeID)) of
    undefined -> undefined;
    Node -> Node
  end.
  
store_way(Way=#way{id=ID, refs=Refs}) ->
  store_nodeids2wayid(Refs, ID),
  merle:set(way_key(ID), Way).
  
store_node(Node=#node{id=ID}) ->
  merle:set(node_key(ID), Node).

store_nodeids2wayid([], _WayID) ->
  success;

store_nodeids2wayid([FirstNodeID|Rest], WayID) ->
  store_nodeid2wayid(FirstNodeID, WayID),
  store_nodeids2wayid(Rest, WayID).
  
store_nodeid2wayid(NodeID, WayID) ->
  merle:set(node2way_key(NodeID), [WayID|node2wayids(NodeID)]).
  
node_key(ID) ->
  "node_" ++ ID.
  
way_key(ID) ->
  "way_" ++ ID.
  
node2way_key(ID) ->
  "node2way" ++ ID.