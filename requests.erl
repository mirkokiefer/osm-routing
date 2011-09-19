-module(requests).
-export([route/2, route_description/2]).

route(SourceID, TargetID) ->
  astar:shortest_path(SourceID, TargetID).

route_description(SourceID, TargetID) ->
  [{path, Path}, _D, _S] = astar:shortest_path_with_distances(SourceID, TargetID),
  PathWithAngles = geodata:path_angles(Path),
  route_description_recursive(PathWithAngles, {undefined, 0}, []).
  
route_description_recursive([[{node, Node}, {distance, Distance}, {angle, Angle}]], {_PreviousWay, PreviousDistance}, Output) ->
  NewOutput = [[{node, Node}, {distance, Distance}, {angle, Angle}, {walk, at_destination(Distance-PreviousDistance)},
    {direction, ""}]|Output],
  Directions = lists:reverse(NewOutput),
  Directions;
  
route_description_recursive([[{node, Node}, {distance, Distance}, {angle, Angle}], Next|Rest], {PreviousWay, PreviousDistance}, Output) ->
  [{node, NextNode}, _, _] = Next,
  Way = geodata:extract_way_tag("name", geodata:connecting_way(Node, NextNode)),
  LogicalDirection = angle_to_direction(Angle),
  {NewOutput, NewPreviousDistance} = case {Way, LogicalDirection} of
    {PreviousWay, straight} -> {Output, PreviousDistance};
    _Any -> RelativeDistance = Distance - PreviousDistance,
      Walk = if
        RelativeDistance > 0 -> distance_to_textual_direction(RelativeDistance);
        true -> ""
      end,
      Direction = string:join([textual_direction(LogicalDirection), textual_direction(into), Way], ""),
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
  string:join([textual_direction(follow), utils:float_to_string(Distance), textual_direction(unit)], "").
  
at_destination(Distance) ->
  string:join([textual_direction(destination), utils:float_to_string(Distance), textual_direction(unit)], "").
  
textual_direction(Direction) ->
  case Direction of
    start -> "Sie starten in ";
    destination -> "Sie haben Ihr Ziel erreicht in ";
    straight -> "Gehen Sie geradeaus ";
    left -> "Biegen Sie nach links ab ";
    right -> "Biegen Sie nach rechts ab ";
    into -> "in ";
    follow -> "Folgen Sie der Strasse fuer ";
    unit -> "m"
  end.