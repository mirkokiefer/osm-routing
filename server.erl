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
  try geodata:route(list_to_atom(Source), list_to_atom(Target)) of
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
  try geodata:route_annotated (list_to_atom(Source), list_to_atom(Target)) of
    [{path, List}, {distance, Distance}, {stats, Stats}] ->
      Coords = [nodes_to_coords(Path) || [{way, _}, {nodes, Path}, {angle, _}] <- List],
      Res = [{route, Coords}, {distance, Distance}, {stats, Stats}],
      Json = binary_to_list(iolist_to_binary(mochijson2:encode(Res))),
      Body = io_lib:format("~s", [Json]),
      Req:ok({"text/plain", Body})
  catch
    _:X -> io:format("~p~n", [X])
  end;

respond("/map", _Params, Req) ->
  Req:serve_file("ui.html", "/Users/mirko/Desktop/Code/Projects/routing/www");

respond(Path, _Params, Req) ->
  FileName = lists:nthtail(1, Path),
  Req:serve_file(FileName, "/Users/mirko/Desktop/Code/Projects/routing/www").
  
nodes_to_coords(Path) ->
  [[{lat, Lat}, {lon, Lon}] || {Lat, Lon} <- geodata:nodes_to_coords(Path)].