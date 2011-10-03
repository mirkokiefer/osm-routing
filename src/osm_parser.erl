
% OSM Parser and DB generator
%
% Author: Mirko Kiefer
%
% This module is able to parse an .osm file and writes it out as a serialized ets database file.

-module(osm_parser).
-export([read/1]).

-include("../includes/routing.hrl").

-record(way_tags, {
  highway = undefined,
  name = undefined,
  ref = undefined,
  other = []
}).

read(File) ->
  store:init(),
  
  xml_parser:parse_file(File, fun event_ways/2),
  xml_parser:parse_file(File, fun event_nodes/2),
  
  store:serialize(),
  store:stop(),
  success.

% handle parser callbacks for ways
event_ways({startElement, _, "way", _, Attributes}, _) ->
  [ID] = filterAttributes(Attributes, ["id"]),
  #way{id=list_to_atom(ID)};
  
event_ways({startElement, _, "tag", _, Attributes}, State) ->
  [K, V] = filterAttributes(Attributes, ["k", "v"]),
  case State of
    #way{tags=Tags} -> State#way{tags=[{K, V} | Tags]};
    _Any -> State
  end;

event_ways({startElement, _, "nd", _, Attributes}, State=#way{refs=Refs}) ->
  [Ref] = filterAttributes(Attributes, ["ref"]),
  State#way{refs=[list_to_atom(Ref) | Refs]};

event_ways({endElement, _, "way", _}, State) ->
  way_read(State),
  [];

event_ways(_Event, State) ->
  State.

% process read way
way_read(Way=#way{tags=Tags}) ->
  TagsRecord = way_tags_to_record(Tags),
  case valid_way(TagsRecord) of
    true ->
      % store additional tag for name to be used in routing descriptions
      Tags1 = [{"routing_name", way_name(TagsRecord)}|Tags],
      store:store_way(Way#way{tags=Tags1});
    false -> ignore
  end.
  
way_name(Tags) ->
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

% handle parser callbacks for nodes
event_nodes({startElement, _, "node", _, Attributes}, _) ->
  [ID, Lat, Lon] = filterAttributes(Attributes, ["id", "lat", "lon"]),
  #node{id=list_to_atom(ID), lat=Lat, lon=Lon};

event_nodes({startElement, _, "tag", _, Attributes}, State) ->
  [K, V] = filterAttributes(Attributes, ["k", "v"]),
  case State of
    #node{tags=Tags} -> State#node{tags=[{K, V} | Tags]};
    _Any -> State
  end;

event_nodes({endElement, _, "node", _}, State) ->
  node_read(State),
  [];

%% Catch-all. Pass state on as-is
event_nodes(_Event, State) ->
  State.

% process read node
node_read(Node=#node{id=ID}) ->
  % check if node is used within a way
  case store:node2wayids(ID) of
    [] -> ignore;
    _Any -> store:store_node(Node)
  end.

% helper functions
filterAttributes(Attributes, FilterAttributes) ->
  [lists:nth(1, [Value || {_, Attr, _, _, Value} <- Attributes, Attr == FilterAttr]) ||
      FilterAttr <- FilterAttributes].

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