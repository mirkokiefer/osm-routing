
-record(node, {
  id,
  lat,
  lon,
  tags
}).

-record(way, {
  id,
  tags,
  refs
}).

-record(route, {
  path,
  distance,
  stats
}).