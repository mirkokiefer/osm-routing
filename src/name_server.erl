-module(name_server).
-export([start/0,stop/0,lookup_name/1]).


start() ->
  ets:new(osm_names_to_nodes,[named_table, set, public]),    %%Tabelle wird angelegt; aus ets mudule new funktion nehmen
  First = ets:first(osm_nodes),   %%erste node aus osm wird genommen; über first funktion, die im ets module definiert ist
  extract_nodes(First),
  success.         %% %%rufe extract_nodes funktion auf mit dem anfangswert, das über First erhalten

stop() ->
  ets:delete(osm_names_to_nodes).     

extract_nodes(Previous) ->       %% rekursive Funktion, die alle nodes durchläuft
  Next = ets:next(osm_nodes, Previous),  %%(next und first sind Funktionen, die man im ets benutzen kann; bis jetzt kann man nur duchlaufen)
  
  case Next of                     %% pogramm stürzt ab, weil nicht weiß, was zu tun, wenn keine nodes mehr da; deshalb case                
    '$end_of_table' -> stopp;      %% wenn next funktion kein next mehr hat, gibt sie das zitat aus, was bedeutet, dass stopp
    Any ->                         %% wenn nicht, dann durchlaufe alle nodes
      check_node(Next),              %% unten definiert; soll erst nur name und ids extrahieren und dann alle nodes mit genannten eigschaften anzeigen
      extract_nodes(Next)         %% soll alle nodes zeigen() (ist die rekursive funktion aus oben), Funktionsaufruf
  end.
  
log(Element) ->                          
  io:format("Log: ~p~n", [Element]).

check_node(NodeID) ->                           %%Funktion, um auf Konsole node auszugeben
  Node = store:lookup_node(NodeID),           %%aus store module die lookup_node funktion
  {node,ID,Lat,Lon,Attributes} = Node,        %% node + Info erscheinen im Tupel in der Konsole 
  case extract_name(Attributes) of
    [] -> ignore;
    [Any] -> store_name(Any,ID)           %%log ist rausgemacht, anstattdessen store_name hinzugefügt (aus store)(Any für irgendein name +id)
  end.
  
extract_name(Attributes) ->                           
  [Value||{Key,Value}<-Attributes, Key == "name"].    

store_name(Name,ID) ->                                %% aus store module; schreiben dadurch in die Datenbamk rein; Info´s aus Konsole in Tabelle
  ets:insert(osm_names_to_nodes, {string:to_lower(Name),ID}).%%name immer klein

lookup_name(Name) ->
 Result = ets:lookup(osm_names_to_nodes,string:to_lower(Name)),      %% können über Funktion in DB reinschauen;wenn du namen eingibst, kommst du auch zur id des 
 Result.                                             %%namens; Fkt. Aufruf; bevor er namen sucht, klein machen

