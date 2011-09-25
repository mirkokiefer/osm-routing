
% API requests
%
% Author: Mirko Kiefer
%
% This module is called by the server and implements the API - mainly the translation of route data to textual directions. 

-module(requests).
-export([route/2, route_description/2]).

-include("../includes/routing.hrl").

route(SourceID, TargetID) ->
  astar:shortest_path(SourceID, TargetID).

route_description(SourceID, TargetID) ->
  #route{path=Path} = astar:shortest_path_with_distances(SourceID, TargetID),
  PathWithAngles = geodata:path_angles(Path),
  route_description_recursive(PathWithAngles, {undefined, 0}, []).
  
route_description_recursive([[{node, Node}, {distance, Distance}, {angle, Angle}]], {_PreviousWay, PreviousDistance}, Output) ->
  NewOutput = [[{node, Node}, {distance, Distance}, {angle, Angle}, {walk, at_destination(Distance-PreviousDistance)},
    {direction, ""}]|Output],
  Directions = lists:reverse(NewOutput),
  Directions;
  
route_description_recursive([[{node, Node}, {distance, Distance}, {angle, Angle}], Next|Rest], {PreviousWay, PreviousDistance}, Output) ->
  [{node, NextNode}, _, _] = Next,
  Way = geodata:way_tag("routing_name", geodata:connecting_way(Node, NextNode)),
  LogicalDirection = angle_to_direction(Angle),
  {NewOutput, NewPreviousDistance} = case {Way, LogicalDirection} of
    {PreviousWay, straight} -> {Output, PreviousDistance};
    _Any -> RelativeDistance = Distance - PreviousDistance,
      Walk = if
        RelativeDistance > 0 -> distance_to_textual_direction(RelativeDistance);
        true -> ""
      end,
      Direction = join_string([
        textual_direction(LogicalDirection),
        textual_direction(into),
        keyword(textual_way_name(Way))
      ]),
      NewOutput1 = [[{node, Node}, {distance, Distance}, {angle, Angle}, {walk, Walk}, {direction, Direction}]|Output],
      {NewOutput1, Distance}
  end,
  route_description_recursive([Next|Rest], {Way, NewPreviousDistance}, NewOutput).
  
angle_to_direction(Angle) ->
  if
    Angle > 30 -> left;
    Angle < -30 -> right;
    true -> straight
  end.
  
distance_to_textual_direction(Distance) ->
  join_string([textual_direction(follow), utils:float_to_string(Distance), textual_direction(unit)]).
  
at_destination(Distance) ->
  join_string([textual_direction(destination), utils:float_to_string(Distance), textual_direction(unit)]).

join_string(List) ->
  string:join(List, "").
  
keyword(Text) ->
  join_string(["<strong>", Text, "</strong>"]).

textual_direction(Direction) ->
  case Direction of
    start -> "Sie starten in ";
    destination -> "Sie haben Ihr Ziel erreicht in ";
    straight -> join_string(["Gehen Sie ", keyword("geradeaus"), " "]);
    left -> join_string(["Biegen Sie nach ", keyword("links"), " ab "]);
    right -> join_string(["Biegen Sie nach ", keyword("rechts"), " ab "]);
    into -> "in ";
    follow -> "Folgen Sie der Strasse fuer ";
    unit -> "m"
  end.
  
textual_way_name(Name) ->
  case Name of
    "track" -> "den Feldweg";
    "cycleway" -> "den Fahrradweg";
    "pedestrian" -> "die Fussgängerzone";
    "footway" -> "den Fussweg";
    "road" -> "die Strasse";
    "tertiary" -> "die Strasse";
    "path" -> "den Pfad";
    "secondary" -> "die Landstrasse";
    "primary" -> "die Bundesstrasse";
    "service" -> "die Zugangsstrasse";
    "unclassified" -> "den Weg";
    "steps" -> "Richtung Treppen";
    Any -> Any
  end.