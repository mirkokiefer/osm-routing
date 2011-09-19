function getParameterByName(name) {
  var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
  return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
}
  
var source = getParameterByName('source');
var target = getParameterByName('target');

function initialize() {
  $.get('route?source=' + source + '&target=' + target, function(json) {
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
     
    var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
    var path = new google.maps.Polyline({
      path: coords,
      map: map,
      strokeColor: "#FF0000",
      strokeOpacity: 1.0,
      strokeWeight: 2
    });
    
    var index = 1;
    /*markerCoords.forEach(function(coord) {
      var options = {
        position: coord,
        map: map,
        title: index.toString()
      };
      new google.maps.Marker(options);
      index++;
    });*/
  });
}