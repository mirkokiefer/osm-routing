/*
  UI logic
  
  Author: Johannes Auer, Haykuhi Jaghinyan
  
  Accessing the backend through HTTP requests, drawing the map and route using Google Maps, displaying the route description.
*/

var developMode = false;
var develop = function() {
  return developMode;
}

var getParameterByName = function(name) {
  var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
  return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
};

var displayMap = function(from, to) {                                   //
  $.get('route?from=' + from + '&to=' + to, function(json) {
    var data = JSON.parse(json);
    var coords = data.route.map(function(each) {
      return new google.maps.LatLng(each.lat, each.lon);
    });
    
    var myLatLng = new google.maps.LatLng(data.route[0].lat, data.route[0].lon);
    var myOptions = {
      zoom: 14,
      center: myLatLng,
      mapTypeId: google.maps.MapTypeId.ROADMAP
    }; 
    var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions); //lege Karte an
    
    var path = new google.maps.Polyline({              //zeichnen Linie
      path: coords,
      map: map,
      strokeColor: "#FF0000",
      strokeOpacity: 1.0,
      strokeWeight: 2
    });
    
    $.get('route_description?from=' + from + '&to=' + to, function(routeDescriptionJson) {   //  function(routeDescriptionJson) ist call back funktion
      var routeDescription = JSON.parse(routeDescriptionJson).description; //die erst gerufen wird, wenn alle Daten im Json Format schon da
      var index = 1;
      routeDescription.forEach(function(each) {
        var options = {
          position: new google.maps.LatLng(each.location.lat, each.location.lon),
          map: map,
          title: index.toString()
        };
        new google.maps.Marker(options); // lege Marker an
        index++;
      });
      displayRouteDescription(routeDescription, {time: data.time, distance: data.distance});
    });
  });
};

var displayEmptyMap = function() {                                              
  //focus on Heidelberg
  var myLatLng = new google.maps.LatLng(49.3970139, 8.679436);
  var myOptions = {
    zoom: 14,
    center: myLatLng,
    mapTypeId: google.maps.MapTypeId.ROADMAP
  }; 
  var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
};

var displayRouteDescription = function(data, stats) {
  var index = 0;
  var last = data.pop();
  var text = data.map(function(each) {
    index++;
    var developModeString = '';
    if(develop()) {
      developModeString = ' (node: ' + each.location.node + ')';    
    }
    return each.walk + '<br><br>' + new String(index) + developModeString + ': ' + each.direction + '<br>';
  });
  text.push(last.walk);
  text.push("<br><br>Distanz: " + (stats.distance/1000).toFixed(2) + " km<br>");
  text.push("Routenberechnung: " + new String(stats.time/1000000) + " Sekunden");
  $('#directions').html(text.join(''));
};

var initForm = function() {               
  $('#submit').click(function() {
    var from = $("#from").val();
    var to = $("#to").val();
    window.location = '?from=' + from + '&to=' + to;
  });
}

$(function() {
  var from = getParameterByName('from');
  var to = getParameterByName('to');
  $("#from").val(from);
  $("#to").val(to);
  initForm();

  $.get("name_server?name="+ from, function(fromJson) {
    var fromData = JSON.parse(fromJson);
    var fromId = fromData[0];
    $.get("name_server?name="+ to, function(toJson) {
      var toData = JSON.parse(toJson);
      var toId = toData[0];
      console.log([fromId,toId])
      if(fromId && toId) {
        displayMap(fromId, toId);
      } else {
        displayEmptyMap();
      } 
    });    
  });
});
