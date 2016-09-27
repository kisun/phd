@extends('layouts.app')

@section('content')
  <div class="page">
    <div class="page-header">
      <h3>Arrival/Departure Delays in Auckland</h3>
    </div>

    <div id="map" style="height:70vh"></div>
  </div>
@endsection


@section('endmatter')
  <script>
    function initMap() {
      var pos = {lat: -36.8485, lng: 174.7633};

      var map = new google.maps.Map(document.getElementById('map'), {
        center: pos,
        zoom: 11,
        disableDefaultUI: true
      });

      var stops = [],
          cutoff = 600,
          low = [151, 83, 34],
          high = [5, 69, 54];

      function updateMap(set = false) {
        $.get({
          url: '/api/delays',
          success: function (data) {
            if (stops.length > 0) {
              for (var i = 0; i < stops.length; i++) {
                stops[i].setMap(null);
              }
              stops = [];
            }

            for (var i = 0; i < data.length; i++) {
              pos = new google.maps.LatLng(data[i].lat, data[i].lon);
              var fraction = 0.5 - Math.max(Math.min(data[i].delay, cutoff), (-cutoff)) / 1200;
              var color = interpolateHsl(low, high, fraction);
              stops[i] = new google.maps.Marker({
                position: pos,
                map: map,
                icon: {
                  path: google.maps.SymbolPath.CIRCLE,
                  strokeWeight: 0.5,
                  strokeColor: color,
                  fillColor: color,
                  fillOpacity: 1,
                  scale: 5
                }
              });
            }
          }
        });
      }
      function interpolateHsl(lowHsl, highHsl, fr) {
        /** Code taken from
          * https://developers.google.com/maps/documentation/javascript/examples/layer-data-quakes
          **/
        var color = [];
        for (var i = 0; i < 3; i++) {
          color[i] = (highHsl[i] - lowHsl[i]) * fr + lowHsl[i];
        }
        return 'hsl(' + color[0] + ',' + color[1] + '%,' + color[2] + '%)';
      }

      updateMap(true);
      setInterval(updateMap, 30000);
    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
