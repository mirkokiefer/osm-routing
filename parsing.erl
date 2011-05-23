-module(parsing).
-export([run/0]).

run() ->
  {ok, Xml} = file:read_file("data/map.osm"),
  erlsom:parse_sax(Xml, [], fun event/2).

parseAttributes(Attributes) ->
  [{Attr, Value} || {_, Attr, _, _, Value} <- Attributes].

filterAttributes(Attributes, FilterAttributes) ->
  ParsedAttributes = parseAttributes(Attributes),
  [lists:nth(1, [Value || {Attr, Value} <- ParsedAttributes, Attr == FilterAttr]) ||
      FilterAttr <- FilterAttributes].

event(Event = {startElement, _, "node", _, Attributes}, _) ->
  ParsedAttributes = parseAttributes(Attributes),
  [ID, Lat, Lon] = filterAttributes(Attributes, ["id", "lat", "lon"]),
  State = {{id, ID, lat, Lat, lon, Lon}, []},
  State;

event(Event = {startElement, _, "tag", _, Attributes}, _State = {Node, Details}) ->
  ParsedAttributes = parseAttributes(Attributes),
  [K, V] = filterAttributes(Attributes, ["k", "v"]),
  NewState = {Node,[{attr, K, V} | Details]},
  NewState;

event(Event = {endElement, _, "node", _}, State) ->
  io:format("end node: ~p~n", [State]),
  [];

event(Event = {startElement, _, "way", _, Attributes}, _) ->
  ParsedAttributes = parseAttributes(Attributes),
  [ID] = filterAttributes(Attributes, ["id"]),
  State = {{id, ID}, []},
  State;

event(Event = {startElement, _, "nd", _, Attributes}, _State = {Node, Details}) ->
  ParsedAttributes = parseAttributes(Attributes),
  [Ref] = filterAttributes(Attributes, ["ref"]),
  NewState = {Node,[{ref, Ref} | Details]},
  NewState;

event(Event = {endElement, _, "way", _}, State) ->
  io:format("end way: ~p~n", [State]),
  [];

%% Catch-all. Pass state on as-is
event(Event, State) ->
  %% io:format("~p~n", [Event]),
  State.