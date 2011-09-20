var getParameterByName = function(name) {
  var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
  return match && decodeURIComponent(match[1].replace(/\+/g, ' '));
};

var displayMap = function(from, to) {
  $.get('route?from=' + from + '&to=' + to, function(json) {
    var data = JSON.parse(json);
    var coords = data.route.map(function(each) {
      return new google.maps.LatLng(each.lat, each.lon);
    });
    console.log(data);
    
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
    
    
    data.route.forEach(function(each) {
      var options = {
          position: new google.maps.LatLng(each.lat, each.lon),
          map: map,
          title: each.node + ' ' + new String(each.lat) + ' ' + new String(each.lon),
        };
        new google.maps.Marker(options);
    });
    
    $.get('route_description?from=' + from + '&to=' + to, function(routeDescriptionJson) {
      var routeDescription = JSON.parse(routeDescriptionJson).description;
      var index = 1;
      displayRouteDescription(routeDescription);
      var shape = {
        coord: [1, 1, 1, 20, 18, 20, 18 , 1],
        type: 'poly'
      };
      var image = new google.maps.MarkerImage('http://code.google.com/apis/maps/documentation/javascript/examples/images/beachflag.png',
      new google.maps.Size(20, 32),
      new google.maps.Point(0,0),
      new google.maps.Point(0, 32));
      routeDescription.forEach(function(each) {
        var options = {
          position: new google.maps.LatLng(each.location.lat, each.location.lon),
          map: map,
          title: index.toString(),
          icon:image,
          shape:shape
        };
        new google.maps.Marker(options);
        index++;
      });
    });
  });
};

var displayEmptyMap = function() {
  //focus on Heidelberg
  var myLatLng = new google.maps.LatLng(49.3970139, 8.679436);
  var myOptions = {
    zoom: 14,
    center: myLatLng,
    mapTypeId: google.maps.MapTypeId.ROADMAP,
    title: "abc"
  }; 
  var map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);
};

var displayRouteDescription = function(data) {
  var index = 0;
  var text = data.map(function(each) {
    index++;
    return new String(index) + ' (' + each.location.node + ' - ' + new String(each.angle) + '): ' + each.walk + '<br><br>' + each.direction + '<br>';
  });
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
  displayMap(from, to);
  if(from && to) {
    displayMap(from, to);
  } else {
    displayEmptyMap();
  }
  initForm();
});