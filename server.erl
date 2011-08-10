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
  
respond("/route", [{"source", Source}, {"target", Target}], Req) ->
  try requests:route(list_to_atom(Source), list_to_atom(Target)) of
    [{path, Path}, {distance, Distance}, {stats, Stats}] ->
      Coords = nodes_to_coords(Path),
      Res = [{route, Coords}, {distance, Distance}, {stats, Stats}],
      Json = binary_to_list(iolist_to_binary(mochijson2:encode(Res))),
      Body = io_lib:format("~s", [Json]),
      Req:ok({"text/plain", Body})
  catch
    _:X -> io:format("~p~n", [X])
  end;
  
respond("/route_annotated", [{"source", Source}, {"target", Target}], Req) ->
  try requests:route_annotated(list_to_atom(Source), list_to_atom(Target)) of
    [{path, List}, {distance, Distance}, {stats, Stats}] ->
      ExtractNodes = fun(Path) -> [Node || [{node, Node}, {distance, _}] <- Path] end,
      Coords = [nodes_to_coords(ExtractNodes(Path)) || [{way, _}, {nodes, Path}, {angle, _}] <- List],
      Res = [{route, Coords}, {distance, Distance}, {stats, Stats}],
      Json = binary_to_list(iolist_to_binary(mochijson2:encode(Res))),
      Body = io_lib:format("~s", [Json]),
      Req:ok({"text/plain", Body})
  catch
    _:X -> io:format("~p~n", [X])
  end;
  
respond("/route_description", [{"source", Source}, {"target", Target}], Req) ->
  try requests:route_description(list_to_atom(Source), list_to_atom(Target)) of
    Description ->
      FormattedDescription = [{[{way, list_to_binary(Way)}, Distance, {direction, list_to_binary(Direction)},
        {walk, list_to_binary(Walk)}]} ||
        [{way, Way}, Distance, {direction, Direction}, {walk, Walk}] <- Description],
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
  
nodes_to_coords(Path) ->
  [[{lat, Lat}, {lon, Lon}] || {Lat, Lon} <- geodata:nodes_to_coords(Path)].