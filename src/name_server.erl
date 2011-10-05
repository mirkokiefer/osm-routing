-module(name_server).
-export([start/0,stop/0,lookup_name/1,extract_nodes/1]).


start() ->
  ets:new(osm_names_to_nodes,[named_table, set, public]),    %%Tabelle wird angelegt; [stehen objects drin]; set heißt ein Schlüssel zu jedem Objekt; 
  First = ets:first(osm_nodes),   %%public:Any process may read or write to the table.
  extract_nodes(First),
  FirstWay = ets:first(osm_ways),  
  extract_ways(FirstWay),
  success.   
      
stop() ->
  ets:delete(osm_names_to_nodes).
  

extract_nodes(CurrentNode) ->       %%Rekursive Funktion: rufe extract_nodes mit aktuellen Node auf
  case CurrentNode of                                 
    '$end_of_table' -> stop;     %%falls Tabelle zu Ende  -> stop
    Any ->                         
      check_node(CurrentNode),   
      NextNode = ets:next(osm_nodes,CurrentNode),              
      extract_nodes(NextNode)
  end.      
  
log(Element) ->                          %%Funktion, um auf Konsole node auszugeben
  io:format("Log: ~p~n", [Element]).

check_node(NodeID) ->                         %%Funktion, die 
  Node = store:lookup_node(NodeID),           %%aus store module die lookup_node funktion; lookup funktion gibt mir dir nodes aus
  {node,ID,Lat,Lon,Attributes} = Node,        %% node + Info erscheinen im Tupel in der Konsole 
  case extract_name(Attributes) of
    [] -> ignore;                             %%falls Attribut leer oder keinen Namen
    [Any] -> store_name(Any,[ID])              %%sonst rufe store_name auf
  end.                                     
  
extract_name(Attributes) ->                           %%list comprehension
  [Value||{Key,Value}<-Attributes, Key == "name"].    

store_name(Name,ID) ->                                %%  schreiben in die Datenbank rein; speichern dadurch
  ets:insert(osm_names_to_nodes, {string:to_lower(Name),ID}).%%name immer klein

lookup_name(Name) ->
  ets:lookup(osm_names_to_nodes,string:to_lower(Name)).    %% können über Funktion in DB reinschauen;wenn du namen eingibst, kommst du auch zur id des 
                                             %%namens; Fkt. Aufruf; bevor er namen sucht, klein machen


extract_ways(CurrentNode) ->      
 case CurrentNode of                                    
    '$end_of_table' -> stop;      
    Any ->                        
      check_ways(CurrentNode), 
      NextNode = ets:next(osm_ways, CurrentNode),             
      extract_ways(NextNode)        
  end.

check_ways(WayID) ->                       
  Way = store:lookup_way(WayID),        %% in neue Tabelle die ways reinschreiben   
  {way,ID,Attributes,Nodes} = Way,  
  case extract_name(Attributes) of
    [] -> ignore;
    [Any] -> store_name(Any,Nodes)           
  end.

                                            


