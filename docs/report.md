# OSM Routing

## Projektziel

## Architektur
Im folgenden wird eine Übersicht über die Architektur des Systems gegeben. Dazu gehören ein Diagramm über die einzelnen Softwaremodule, eine Beschreibung des Ablaufs einer Anfrage sowie eine Übersicht über die verwendeten Erlang Module.

### Diagramm
![Overview](routing.pdf)

### Bearbeitung einer Anfrage an das System

### Erlang Module
#### astar
#### geodata
#### osm_parser
#### priority_queue
#### requests
#### routing
#### server
#### store

## Softwarekomponenten
### Programmiersprache
Die Wahl der Programmiersprache viel auf Erlang.

Pro:

* Virtuelle Maschine:
Erlang läuft auf einer VM. Damit ist es sehr leicht möglich ein Programm zu ändern, während es läuft, ohne es komplett neu zu kompilieren und neu zu starten. Dies erhöht die Produktivität. 

* Referenzielle Transparenz:
Da der einzige Effekt einer nebeneffektfreien Funktion ihr Rückgabewert ist, sind Sie leichter zu verstehen als Prozeduren.

* Deklarative Sprache:
Erlang ist eine Deklarative Sprache. Im vergleich zu imperativen Sprachen führt das zu kürzeren, leichter verständlichen Programmen.

* Leichtgewichtige Prozesse:
Prozesse sind Teil der Erlang VM und nicht Teil des Betriebssystems. Dadurch sind Sie wesentlich leichtgewichtiger als OS Prozesse oder sogar Threads. Auf einem handelsüblichen Rechner ist es möglich mehrere hundertausend Erlang Prozesse laufen zu lassen. Damit ist Erlang optimal für ein asynchrones und ausfallsicheres System geeignet.


Kontra:

* Performancekritischer, sequentieller Code:
Durch die Indirektion der VM geht ein Teil der Performance der Hardware verloren. Obwohl der grösste Teil des Systems durch IO Operationen dominiert wird, gibt es kleine performancekritische, sequentielle Code Stücke (z.B. der routing Algorithmus). Hier könnte man durch eine Hardwarenhe Sprache wie C eine verbesserte Abtwortzeit erreichen.

### Datenbank
### Algorithmus
### HTTP Server

## Weiterentwicklung
### Geochouch
### Polymaps
### Algorithmus