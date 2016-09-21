@extends('layouts.app')

@section('content')
  <div class="page">
    <h3>{{ $trip->route->short_name }} <small>{{ $trip->route->long_name }}</small></h3>

    <hr>
    <div id="map" style="height:70vh"></div>
    <hr>

  </div>
@endsection

@section('endmatter')
  <script>
    function initMap() {

      var data = [
        @foreach ($shape as $point)
          { lat: {{ $point->lat }}, lng: {{ $point->lon }} },
        @endforeach
      ];

      var stops = {!! $stops->toJson() !!};

      var pos = {
        lat: {{ $trip->vehicle_position->position_latitude }},
        lng: {{ $trip->vehicle_position->position_longitude }}
      };

      var map = new google.maps.Map(document.getElementById('map'), {
        center: pos,
        zoom: 15,
        disableDefaultUI: true
      });
      var shapePath = new google.maps.Polyline({
        path: data,
        geodesic: true,
        strokeColor: '{{ ($trip->route->color) ? $trip->route->color : "#000099" }}',
        strokeOpacity: 1.0,
        strokeWeight: 2
      });
      shapePath.setMap(map);

      var infoWindow = new google.maps.InfoWindow,
          stopmarkers = [];

      for (var i = 0; i < stops.length; i++) {
        pos = new google.maps.LatLng(parseFloat(stops[i].stop.lat),
                                     parseFloat(stops[i].stop.lon));
        stopmarkers[i] = new google.maps.Marker({
          position: pos,
          icon: {
            path: google.maps.SymbolPath.CIRCLE,
            scale: 3,
            strokeColor: '{{ ($trip->route->color) ? $trip->route->color : "#000099" }}'
          },
          draggable: false,
          map: map,
          zIndex: 1
        });
      }

      var marker,
          time = {{ $trip->vehicle_position->timestamp }},
          particles = [],
          particleTime = 0,
          infowindow = new google.maps.InfoWindow();

      function infopanel() {
        infowindow.close();
        infowindow.setContent(getContent(this.info));
        infowindow.open(map, this);
      }

      function getContent(vehicle) {
        var html = '';
        html += '<h4>{{ $trip->route->short_name }}: {{ $trip->route->long_name }}</h4>';
        html += '<p>Departs {{ $trip->start_time()->format('g:i a') }}</p>';
        // html += '<strong><a href="/route/' + position.route.short_name + '">Route #'
        //      + position.route.short_name + '</a> - ' + position.direction_id + '</strong>';
        // html += '<p>' + position.route_long_name + '</p>';
        // var delay = position.trip_update.stop_time_updates.arrival_delay ||
        //             position.trip_update.stop_time_updates.departure_delay || '';
        // if (delay.length > 0) {
        //   var delayTxt;
        //   if (delay == 0) {
        //     delayTxt = 'On schedule';
        //   } else if (delay > 0) {
        //     delayTxt = secondsToTime(delay) + ' seconds ahead of schedule';
        //   } else {
        //     delayTxt = secondsToTime(-delay) + ' seconds behind schedule';
        //   }
        //   html += '<p>' + delayTxt + ' (' + (position.arrival_delay ? 'arrival' : 'departure') +')</p>';
        // }
        // if (position.age) {
        //   html += '<p class="small">Last position reported ' + position.age + '</p>';
        // }
        html += '<p class="small">Vehicle ID: ' + vehicle.vehicle_id + '</p>';
        return html;
      }

      function updateMap(set = false) {
        $.get({
          url: '/api/vehicle_position/{{ $trip->id }}',
          success: function (data) {
            vehicle = data;
            pos = new google.maps.LatLng(vehicle.position_latitude, vehicle.position_longitude);
            if (typeof marker == 'undefined') {
              marker = new google.maps.Marker({
                position: pos,
                icon: "{{ url('img/bus.png') }}",
                map: map,
                info: vehicle,
                optimized: false,
                animation: google.maps.Animation.DROP,
                zIndex: 5
              });
            } else if (vehicle.timestamp > time) {
              marker.setPosition(pos);
              map.panTo(pos);
            }
            time = vehicle.timestamp;

            // add the particles ... if they exist!
            if (vehicle.particles.length > 0) {
              if (vehicle.particles[1].timestamp > particleTime) {
                if (particles.length > 0) {
                  for (var i = 0; i < particles.length; i++) {
                    particles[i].setMap(null);
                  }
                  particles = [];
                }
                for (var i=0; i<vehicle.particles.length; i++) {
                  particles[i] = new google.maps.Marker({
                    position: new google.maps.LatLng(vehicle.particles[i].lat,
                                                     vehicle.particles[i].lon),
                    icon: {
                      path: google.maps.SymbolPath.CIRCLE,
                      scale: 3,
                      strokeColor: '#ccccff'
                    },
                    draggable: false,
                    optimized: false,
                    map: map,
                    zIndex: 1
                  });
                }
                particleTime = vehicle.particles[1].timestamp;
              }
            }

            google.maps.event.addListener(marker, 'click', infopanel);
            //google.maps.event.addListener(markers[i], 'mouseover', lastStop);
            //google.maps.event.addListener(markers[i], 'mouseout', clearStop);
            //bounds.extend(pos);
          }
        });
      }
      updateMap(true);
      setInterval(updateMap, 1000);
    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
