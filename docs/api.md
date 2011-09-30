# Erlang Routing Interfaces
## routing

### start()
Starts the routing system.

### stop()
Stops the routing system.

### load_osm_data(File)
Loads OpenStreetMap data from a .osm file. The system needs to be stopped first.

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

# Internal Module Interfaces

## astar
### shortest_path(SourceID, TargetID)
Returns a list of nodes, that are the shortest path between 'SourceID' and 'TargetID'.

### shortest_path_with_distances(SourceID, TargetID)
Returns a list of nodes, that are the shortest path between 'SourceID' and 'TargetID'. Each nodes contains the distance from 'SourceID'.

## geodata
### neighbours(NodeID)
Returns the neighbours of 'NodeID'.

### distance(NodeAID, NodeBID)
Returns the distance between 'NodeAID' and 'NodeBID'.

### nodeid_to_coords(NodeID)
Returns the coordinates of 'NodeID'.

### nodes_to_coords(List)
Returns a list of coordinates from the nodes in 'List'.

### path_angles(Path)
Returns a list of angles between the nodes of 'Path'.

### connecting_way(NodeA, NodeB)
Returns the connecting way between 'NodeA' and 'NodeB'.

### way_tag(FilterTag, WayID)
Returns the tag 'FilterTag' of 'WayID'.

##name_server

## osm_parser
### read(File)
Reads an OSM file, creates appropriate ETS tables and serializes them on disk.

## requests
### route(SourceID, TargetID)
Returns a list of nodes, that are the shortest path between 'SourceID' and 'TargetID' nodes.

### route_description(SourceID, TargetID)
Returns a human-readable route description of the shortest path between 'SourceID' and 'TargetID' nodes.

## server
### start()
Starts the Erlang HTTP Server.

### stop()
Stops the Erlang HTTP Server.

## store
### init()
Initializes the store with empty databases.

### start()
Starts the store by loading it from previously serialized files.

### stop()
Stops the store.

### serialize()
Serializes all tables to disk.

### node2ways(NodeID)
Retuns all ways that 'NodeID' is part of.

### lookup_way(WayID)
Returns the data for 'WayID'.

### lookup_node(NodeID)
Returns the data for 'NodeID'.