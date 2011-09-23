# Module APIs


## astar

### shortest_path/2
Returns a list of nodes, that are the shortest path between 'SourceID' and 'TargetID'.

### shortest_path_with_distances/2
Returns a list of nodes, that are the shortest path between 'SourceID' and 'TargetID'. Each nodes contains the distance from 'SourceID'.


## geodata

### neighbours/1
Returns the neighbours of 'NodeID'.

### distance/2
Returns the distance between 'NodeAID' and 'NodeBID'.

### nodeid_to_coords/1
Returns the coordinates of 'NodeID'.

### nodes_to_coords/1
Returns a list of coordinates from the nodes in 'List'.

### path_angles/1
Retunrs a list of angles between the nodes of 'Path'.

### connecting_way/2
Retuns the connecting way between 'NodeA' and 'NodeB'.

### way_tag/2
Returns the tag 'FilterTag' of 'WayID'.


## osm_parser

### read/1
Reads an OSM file, creates appropriate ETS tables, serializes them and write their data on disk.


## processing

### loadData/0
Reads the ETS data from disk and creates the tables.


## requests

### route/2
Returns a list of nodes, that are the shortest path between 'SourceID' and 'TargetID'.

### route_description/2
Returns a detailed description of the shortest path between 'SourceID' and 'TargetID'.


## server

### start/0
Starts the Erlang HTTP Server.

### stop/0
Stops the Erlang HTTP Server.

### loop/1
Gets Called by MochiWeb for each incoming request.

## store

### node2ways/1
Retuns all ways, thats 'NodeID' is part of.

### lookup_way/1
Returns the data for 'WayID'.

### lookup_node/1
Returns the data for 'NodeID'.


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