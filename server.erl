-module(server).

-export([start/0, stop/0, loop/1]).

-define(HTTP_OPTS, [
            {loop, {?MODULE, loop}},
            {port, 2904},
            {name, http_2904}
            ]).

start() ->
  {ok, Http} = mochiweb_http:start(?HTTP_OPTS),
  Http.

stop() ->
  mochiweb_http:stop().

loop(Req) ->
  respond(Req:get(path), lists:sort(Req:parse_qs()), Req).
  
respond("/route", [{"from", From}, {"to", To}], Req) ->
  try requests:route(list_to_atom(From), list_to_atom(To)) of
    [{path, Path}, {distance, Distance}, {stats, Stats}] ->
      Coords = nodes_to_coords(Path),
      Res = {[{route, Coords}, {distance, Distance}, {stats, {Stats}}]},
      {ok, Json} = json:encode(Res),
      Body = io_lib:format("~s", [binary_to_list(Json)]),
      Req:ok({"text/plain", Body})
  catch
    _:X -> io:format("~p~n", [X])
  end;
  
respond("/route_description", [{"from", From}, {"to", To}], Req) ->
  try requests:route_description(list_to_atom(From), list_to_atom(To)) of
    Description ->
      io:format("~p~n", [Description]),
      FormattedDescription = [{[{location, {node_to_coords(NodeID)}}, Distance, {walk, list_to_binary(Walk)},
        {direction, list_to_binary(Direction)}]} ||
        [{node, NodeID}, Distance, _Angle, {walk, Walk}, {direction, Direction}] <- Description],
      NewFDesc = {[{description, FormattedDescription}]},
      {ok, Json} = json:encode(NewFDesc),
      Body = io_lib:format("~s", [binary_to_list(Json)]),
      Req:ok({"text/plain", Body})
  catch
    _:X -> io:format("~p~n", [X])
  end;

respond("/map", _Params, Req) ->
  Req:serve_file("ui.html", filename:absname("www"));

respond(Path, _Params, Req) ->
  FileName = lists:nthtail(1, Path),
  Req:serve_file(FileName, filename:absname("www")).

node_to_coords(Node) ->
  {Lat, Lon} = geodata:nodeid_to_coords(Node),
  [{node, Node}, {lat, Lat}, {lon, Lon}].

nodes_to_coords(Path) ->
  [{[{node, Node}, {lat, Lat}, {lon, Lon}]} || {Node, {Lat, Lon}} <- lists:zip(Path, geodata:nodes_to_coords(Path))].