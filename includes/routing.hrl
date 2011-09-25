
-include("../includes/config.hrl").

-record(node, {
  id,
  lat,
  lon,
  tags=[]
}).

-record(way, {
  id,
  tags=[],
  refs=[]
}).

-record(route, {
  path,
  distance,
  time,
  nodes,
  visited_nodes,
  memory
}).