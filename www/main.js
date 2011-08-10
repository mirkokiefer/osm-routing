function getParameterByName(name) {
  var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
  return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
}
  
var source = getParameterByName('source');
var target = getParameterByName('target');

function initialize() {
  $.get('route_annotated?source=' + source + '&target=' + target, function(json) {
    var data = JSON.parse(json);
    var coordsArray = data.route.map(function(group) {
      return group.nodes.map(function(each) {
        return new google.maps.LatLng(each.lat, each.lon);
      });
    });
    var coords = [];
    coordsArray.forEach(function(group) {
      coords = coords.concat(group);
    });
    var markerCoords = coordsArray.map(function(group) {
      return group[0];
    });
    var lastArray = coordsArray[coordsArray.length-1];
    var target = lastArray[lastArray.length-1];
    markerCoords.push(target);
    
    var myLatLng = new google.maps.LatLng(data.route[0].nodes[0].lat, data.route[0].nodes[0].lon);
    var myOptions = {
      zoom: 14,
      center: myLatLng,
      mapTypeId: google.maps.MapTypeId.ROADMAP
    };
     
    var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
    var path = new google.maps.Polyline({
      path: coords,
      map: map,
      strokeColor: "#FF0000",
      strokeOpacity: 1.0,
      strokeWeight: 2
    });
    
    var index = 1;
    markerCoords.forEach(function(coord) {
      var options = {
        position: coord,
        map: map,
        title: index.toString()
      };
      new google.maps.Marker(options);
      index++;
    });
  });
}