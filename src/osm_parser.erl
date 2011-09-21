-module(osm_parser).
-export([read/1]).

% osm_parser:read("../data/heidelberg_mannheim.osm").
read(File) ->
  {ok, Xml} = file:read_file(File),
  ets:new(osm_nodes, [named_table, set, public]),
  ets:new(osm_ways, [named_table, set, public]),
  erlsom:parse_sax(Xml, [], fun event/2),
  filelib:ensure_dir("../output/"),
  ets:tab2file(osm_nodes, "../output/osm_nodes.tab"),
  ets:tab2file(osm_ways, "../output/osm_ways.tab"),
  delete_tabs().

parseAttributes(Attributes) ->
  [{Attr, Value} || {_, Attr, _, _, Value} <- Attributes].

filterAttributes(Attributes, FilterAttributes) ->
  ParsedAttributes = parseAttributes(Attributes),
  [lists:nth(1, [Value || {Attr, Value} <- ParsedAttributes, Attr == FilterAttr]) ||
      FilterAttr <- FilterAttributes].


event({startElement, _, "node", _, Attributes}, _) ->
  [ID, Lat, Lon] = filterAttributes(Attributes, ["id", "lat", "lon"]),
  State = {{id, list_to_atom(ID), lat, Lat, lon, Lon}, []},
  State;

event({startElement, _, "tag", _, Attributes}, State) ->
  [K, V] = filterAttributes(Attributes, ["k", "v"]),
  case State of
    {Node, Tags} -> NewState = {Node,[{tag, K, V} | Tags]};
    {Node, Tags, Refs} -> NewState = {Node,[{K, V} | Tags], Refs};
    _Any -> NewState = State
  end,
  NewState;

event({endElement, _, "node", _}, State) ->
  %io:format("end node: ~p~n", [State]),
  {{id, ID, lat, Lat, lon, Lon}, Tags} = State,
  ets:insert(osm_nodes, {ID, {lat, Lat}, {lon, Lon}, {tags, Tags}}),
  [];

event({startElement, _, "way", _, Attributes}, _) ->
  [ID] = filterAttributes(Attributes, ["id"]),
  State = {{id, list_to_atom(ID)}, [], []},
  State;

event({startElement, _, "nd", _, Attributes}, _State = {Node, Tags, Refs}) ->
  [Ref] = filterAttributes(Attributes, ["ref"]),
  NewState = {Node, Tags, [list_to_atom(Ref) | Refs]},
  NewState;

event({endElement, _, "way", _}, State) ->
  {{id, ID}, Tags, Refs} = State,
  case lists:any(fun({K, _V})-> K == "name" end, Tags) of
    true -> ets:insert(osm_ways, {ID, {tags, Tags}, {refs, Refs}});
    false -> ignore
  end,
  [];

%% Catch-all. Pass state on as-is
event(_Event, State) ->
  State.
  
delete_tabs() ->
  ets:delete(osm_nodes),
  ets:delete(osm_ways).