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
    Route ->
      Coords = [[{lat, Lat}, {lon, Lon}] || {Lat, Lon} <- geodata:nodes_to_coords(Route)],
      Json = binary_to_list(iolist_to_binary(mochijson2:encode(Coords))),
      Body = io_lib:format("~s", [Json]),
      Req:ok({"text/plain", Body})
  catch
    _:X -> io:format("~p~n", [X])
  end;
  
respond(Path, _Params, Req) ->
  FileName = lists:nthtail(1, Path),
  Req:serve_file(FileName, "/Users/mirko/Desktop/Code/Projects/routing/www").