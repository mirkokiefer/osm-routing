-module(parsing).
-export([run/0]).

run() ->
  {ok, Xml} = file:read_file("data/map.osm"),
  erlsom:parse_sax(Xml, [], fun event/2).


filterAttributes(Attributes, FilterAttributes) ->
  [[Value || {Attr, Value} <- Attributes, Attr == FilterAttr] ||
      FilterAttr <- FilterAttributes].

event(Event = {startElement, _, "node", _, Attributes}, _) ->
  ParsedAttributes = [{Attr, Value} || {_, Attr, _, _, Value} <- Attributes],
  [[ID], [Lat], [Lon]] = [[Value ||
    {Attr, Value} <- ParsedAttributes, Attr == FilterAttr] ||
      FilterAttr <- ["id", "lat", "lon"]],
  State = {{id, ID, lat, Lat, lon, Lon}, []},
  State;

event(Event = {startElement, _, "tag", _, Attributes}, _State = {Node, Details}) ->
  ParsedAttributes = [{Attr, Value} || {_, Attr, _, _, Value} <- Attributes],
  [[K], [V]] = [[Value ||
    {Attr, Value} <- ParsedAttributes, Attr == FilterAttr] ||
      FilterAttr <- ["k", "v"]],
  NewState = {Node,[{K, V} | Details]},
  NewState;

event(Event = {endElement, _, "node", _}, State) ->
  io:format("end node: ~p~n", [State]),
  [];

%% Catch-all. Pass state on as-is    
event(Event, State) ->
  io:format("~p~n", [Event]),
  State.