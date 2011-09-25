
% OSM Parser and DB generator
%
% Author: Mirko Kiefer
%
% This module is able to parse an .osm file and writes it out as a serialized ets database file.

-module(osm_parser).
-export([read/1, way_tags_to_record/1]).

-record(way_tags, {
  highway = undefined,
  name = undefined,
  ref = undefined,
  other = []
}).

read(File) ->
  {ok, Xml} = file:read_file(File),
  ets:new(osm_nodes, [named_table, set, public]),
  ets:new(osm_ways, [named_table, set, public]),
  ets:new(osm_nodes_to_ways, [named_table, bag, public]),
  erlsom:parse_sax(Xml, [], fun event_ways/2),
  erlsom:parse_sax(Xml, [], fun event_nodes/2),
  build_nodes_ways_tab(),
  
  filelib:ensure_dir("../output/"),
  write_tabs(),
  delete_tabs(),
  success.
  
write_tabs() ->
  ets:tab2file(osm_nodes, "../output/osm_nodes.tab"),
  ets:tab2file(osm_ways, "../output/osm_ways.tab"),
  ets:tab2file(osm_nodes_to_ways, "../output/osm_nodes_to_ways.tab").  

delete_tabs() ->
  ets:delete(osm_nodes),
  ets:delete(osm_ways),
  ets:delete(osm_nodes_to_ways).

parseAttributes(Attributes) ->
  [{Attr, Value} || {_, Attr, _, _, Value} <- Attributes].

filterAttributes(Attributes, FilterAttributes) ->
  ParsedAttributes = parseAttributes(Attributes),
  [lists:nth(1, [Value || {Attr, Value} <- ParsedAttributes, Attr == FilterAttr]) ||
      FilterAttr <- FilterAttributes].


event_nodes({startElement, _, "node", _, Attributes}, _) ->
  [ID, Lat, Lon] = filterAttributes(Attributes, ["id", "lat", "lon"]),
  State = {{id, list_to_atom(ID), lat, Lat, lon, Lon}, []},
  State;

event_nodes({startElement, _, "tag", _, Attributes}, State) ->
  [K, V] = filterAttributes(Attributes, ["k", "v"]),
  case State of
    {Node, Tags} -> NewState = {Node,[{tag, K, V} | Tags]};
    {Node, Tags, Refs} -> NewState = {Node,[{K, V} | Tags], Refs};
    _Any -> NewState = State
  end,
  NewState;

event_nodes({endElement, _, "node", _}, State) ->
  %io:format("end node: ~p~n", [State]),
  {{id, ID, lat, Lat, lon, Lon}, Tags} = State,
  ets:insert(osm_nodes, {ID, {lat, Lat}, {lon, Lon}, {tags, Tags}}),
  [];

%% Catch-all. Pass state on as-is
event_nodes(_Event, State) ->
  State.

event_ways({startElement, _, "way", _, Attributes}, _) ->
  [ID] = filterAttributes(Attributes, ["id"]),
  State = {{id, list_to_atom(ID)}, [], []},
  State;
  
event_ways({startElement, _, "tag", _, Attributes}, State) ->
  [K, V] = filterAttributes(Attributes, ["k", "v"]),
  case State of
    {Node, Tags} -> NewState = {Node,[{tag, K, V} | Tags]};
    {Node, Tags, Refs} -> NewState = {Node,[{K, V} | Tags], Refs};
    _Any -> NewState = State
  end,
  NewState;

event_ways({startElement, _, "nd", _, Attributes}, _State = {Node, Tags, Refs}) ->
  [Ref] = filterAttributes(Attributes, ["ref"]),
  NewState = {Node, Tags, [list_to_atom(Ref) | Refs]},
  NewState;

event_ways({endElement, _, "way", _}, State) ->
  {{id, ID}, Tags, Refs} = State,
  TagsRecord = way_tags_to_record(Tags),
  case valid_way(TagsRecord) of
    true -> Tags1 = [{"routing_name", name(TagsRecord)}|Tags],
      ets:insert(osm_ways, {ID, {tags, Tags1}, {refs, Refs}});
    false -> ignore
  end,
  [];

%% Catch-all. Pass state on as-is
event_ways(_Event, State) ->
  State.

way_tags_to_record(WayTags) ->
  way_tags_to_record_recursive(WayTags, #way_tags{}).

way_tags_to_record_recursive([], Record) ->
  Record;

way_tags_to_record_recursive([First|Rest], Record=#way_tags{other=Other}) ->
  Record1 = case First of
    {"highway", Value} -> Record#way_tags{highway=Value};
    {"ref", Value} -> Record#way_tags{ref=Value};
    {"name", Value} -> Record#way_tags{name=Value};
    Any -> Record#way_tags{other=[Any|Other]}
  end,
  way_tags_to_record_recursive(Rest, Record1).
  
name(Tags) ->
  case Tags of
    #way_tags{name=Name} when Name /= undefined -> Name;
    #way_tags{ref=Ref} when Ref /= undefined -> Ref;
    #way_tags{highway=Highway} when Highway /= undefined -> Highway;
    _Any -> "unknown"
  end.
  
valid_way(Tags) ->
  case Tags of
    #way_tags{highway="motorway"} -> false;
    #way_tags{highway=Any} when Any /= undefined -> true;
    _Any -> false
  end.

% build a table to translate Nodes to Ways
build_nodes_ways_tab() ->
  FirstKey = ets:first(osm_ways),
  [{ID, _, {refs, Refs}}] = ets:lookup(osm_ways, FirstKey),
  write_nodes_to_way(Refs, ID),
  build_nodes_ways_tab(ets:next(osm_ways, FirstKey)).

build_nodes_ways_tab('$end_of_table') ->
  true;

build_nodes_ways_tab(NextKey) ->
  [{ID, _, {refs, Refs}}] = ets:lookup(osm_ways, NextKey),
  write_nodes_to_way(Refs, ID),
  build_nodes_ways_tab(ets:next(osm_ways, NextKey)).
  
write_nodes_to_way(Refs, WayID) ->
  lists:foreach(fun(Ref) -> ets:insert(osm_nodes_to_ways, {Ref, WayID}) end, Refs).