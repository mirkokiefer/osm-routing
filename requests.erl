-module(requests).
-export([route/2, route_annotated/2, route_description/2]).

route(SourceID, TargetID) ->
  astar:shortest_path(SourceID, TargetID).
  
route_annotated(SourceID, TargetID) ->
  [{path, Nodes}, D, S] = astar:shortest_path_with_distances(SourceID, TargetID),
  [{path, geodata:group_nodes(Nodes)}, D, S].
  
route_description(SourceID, TargetID) ->
  [{path, Path}, D, _S] = route_annotated(SourceID, TargetID),
  NewPath = geodata:way_distances(Path),
  Description = route_description_recursive(NewPath, []).
  
route_description_recursive([], Output) ->
  [First|Directions] = lists:reverse(Output),
  [{way, FirstWay}, {distance, FirstDistance}, _, FirstWalk] = First,
  NewFirst = [{way, FirstWay}, {distance, FirstDistance},
    {direction, string:join([textual_direction(start), FirstWay], "")}, FirstWalk],
  NewPath = [NewFirst | Directions],
  NewPath;
  
route_description_recursive([[{way, Way}, {distance, Distance}, {angle, Angle}]|Rest], Output) ->
  Direction = string:join([textual_direction(angle_to_direction(Angle)), textual_direction(into), Way], ""),
  ResolvedDirection = [[{way, Way}, {distance, Distance}, {direction, Direction},
    {walk, distance_to_textual_direction(Distance)}]|Output],
  route_description_recursive(Rest, ResolvedDirection).
  
angle_to_direction(Angle) ->
  if
    Angle > 30 -> left;
    Angle < -30 -> right;
    true -> straight
  end.
  
distance_to_textual_direction(Distance) ->
  string:join([textual_direction(follow), utils:float_to_string(Distance), textual_direction(unit)], "").
  
textual_direction(Direction) ->
  case Direction of
    start -> "Sie starten in ";
    straight -> "Gehen Sie geradeaus ";
    left -> "Biegen Sie nach links ab ";
    right -> "Biegen Sie nach rechts ab ";
    into -> "in ";
    follow -> "Folgen Sie der Strasse fuer ";
    unit -> "m"
  end.