# Erlang Modules

## astar
### shortest_path/2
### shortest_path_with_distances/2

## geodata
### edges/1
### distance/2
### nodeid_to_coords/1
### nodes_to_coords/1
### path_angles/1
### connecting_way/2
### way_tag/2

## osm_parser
### read/1

## priority_queue
### new/0
### add/2
### list/1
### remove/2
### remove_all/2
### smallest/1

## processing
### loadData/0

## requests
### route/2
### route_description/2

## server
### start/0
### stop/0
### loop/1

## store
### node2ways/1
### lookup_way/1
### lookup_node/1

## utils
### float_to_string/1
### deg2rad/1
### rad2deg/1
### intersection/2

# HTTP APIs

##Route
	http://domain:port/route?from=StartID&to=DestinationID
Outputs a JSON formatted list of Node IDs for the computed route.

##Route description
	http://domain:port/route_description?from=StartID&to=DestinationID
Outputs a JSON formatted route description including meta-data of the route.

##User Interface
	http://domain:port/map?from=StartID&to=DestinationID

Displays a map and an interface to enter two node IDs. 
On submit a path and a route description is displayed.