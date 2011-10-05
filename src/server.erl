
% HTTP Server
%
% Author: Mirko Kiefer
%
% The implementation of the HTTP server required to interface the system through an HTTP API.
% This module should only implement the server logic and JSON serialization.

-module(server).

-export([start/0, stop/0, loop/1]).

-include("../includes/routing.hrl").

-define(HTTP_OPTS, [
            {loop, {?MODULE, loop}},
            {port, ?SERVER_PORT},
            {name, http_routing}
            ]).

start() ->
  {ok, Http} = mochiweb_http:start(?HTTP_OPTS),
  Http.

stop() ->
  mochiweb_http:stop(http_routing).

loop(Req) ->
  respond(Req:get(path), lists:sort(Req:parse_qs()), Req).
  
respond("/route", [{"from", From}, {"to", To}], Req) ->             %%parameter (pfad;from to; anfrage)
  try requests:route(From, To) of
    #route{path=Path, distance=Distance, time=Time} ->
      Coords = nodes_to_coords(Path),
      Res = {struct, [{route, Coords}, {distance, Distance}, {time, Time}]},
      Json = mochijson2:encode(Res),
      Req:ok({"text/plain;charset=utf-8", Json})
  catch
    _:X -> io:format("~p~n", [X])
  end;
  
respond("/route_description", [{"from", From}, {"to", To}], Req) ->
  try requests:route_description(From, To) of
    Description ->
      FormattedDescription = [{struct, [{location, node_to_coords(NodeID)}, Distance, {walk, encode(Walk)}, {direction, encode(Direction)}]} ||
        [{node, NodeID}, Distance, _Angle, {walk, Walk}, {direction, Direction}] <- Description],
      NewFDesc = {struct, [{description, FormattedDescription}]},
      Json = mochijson2:encode(NewFDesc),
      Req:ok({"text/plain;charset=utf-8", Json})
  catch
    _:X -> io:format("~p~n", [X])
  end;
  
respond("/map", _Params, Req) ->
  Req:serve_file("ui.html", filename:absname("../www"));

respond("/name_server",[{"name",Name}], Req) ->    %% Anfrage an server; haben Namen der Stadt und wollen über lookup_name funkt.die ID
  Result = name_server:lookup_name(Name),
  case Result of
    [] -> Req:ok({"text/plain;charset=utf-8", "not found"}); %% wenn Name nicht exisiert, dann kommt [] aud Konsole und indem Fall "not found auf"browser
    [{_,IDs}] -> 
      Json = mochijson2:encode(IDs),
      Req:ok({"text/plain;charset=utf-8",Json})  %% wenn auf konsole name+id erscheint, dann wandele das die Id(die ein atom ist)in ein   ?????????????????????
  end;                                                                  %%um, da man browser nur string schicken kann (atom id ist in variable Id gespeichert)

  
respond(Path, _Params, Req) ->
  FileName = lists:nthtail(1, Path),
  Req:serve_file(FileName, filename:absname("../www")).

node_to_coords(Node) ->
  {Lat, Lon} = geodata:nodeid_to_coords(Node),
  {struct, [{node, encode(Node)}, {lat, Lat}, {lon, Lon}]}.

nodes_to_coords(Path) ->
  [{struct, [{node, encode(Node)}, {lat, Lat}, {lon, Lon}]} || {Node, {Lat, Lon}} <- lists:zip(Path, geodata:nodes_to_coords(Path))].
  
encode(String) ->
  unicode:characters_to_binary(io_lib:format("~ts", [String])).
