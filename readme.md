# OSM Routing
An Erlang geo-routing system based on OpenStreetMap data providing a HTTP-API.

## Authors
- [Mirko Kiefer](https://github.com/mirkok) (mail@mirkokiefer.com)
- [Haykuhi Jaghinyan](mailto:haikuhi290489@aol.com)
- [Johannes Auer](https://github.com/johannesauer)

## Installation
Get the current source from https://github.com/livelycode/routing.
### Erlang
#### Download
1. Download Erlang/OTP R14B03 at http://www.erlang.org/download.html.
2. Unpack the download.
3. Now cd into the base dir.
#### Build
    $ ./configure
    $ make
#### Install
    $ make install

### Erlsom
#### Download
	$ git clone https://github.com/willemdj/erlsom.git
#### Build
	$ cd erlsom
	$ make
#### Install
Move folder to erlang path.

### MochiWeb
#### Download
	$ git clone https://github.com/mochi/mochiweb.git
#### Build
	$ cd mochiweb
	$ make
#### Install
Move folder to erlang path.

### OSM Routing
Create an App folder and cd into it.
#### Download
	$ erl
#### Build
	$ cd routing/src
	$ erl
	$ c(astar).
	$ c(geodata).
	$ c(osm_parser).
	$ c(priority_queue).
	$ c(processing).
	$ c(requests).
	$ c(routing).
	$ c(server).
	$ c(store).
	$ c(utils).

## Usage
### Start Erlang Interpreter
	$ erl

### Create Database
	$ routing:load_osm_data("/path/to/your/osm/file.osm").

### Start Server
	$ routing:start().

### Stop Server
	$ routing:stop().

## API Documentation

[API Documentation](https://github.com/livelycode/routing/blob/master/api.md)