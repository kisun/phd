@extends('layouts.app')

@section('container-class', 'scrollpage-app')
@section('content')
  <div class="page">
    <h3>{{ $trip->route->short_name }} <small>{{ $trip->route->long_name }}</small></h3>

    <hr>
    <div class="row">
      <div class="col-md-8">
        <div id="map" style="height:70vh"></div>
      </div>

      <div class="col-md-4">
        <h5>Schedule Delay</h5>

        <div id="scheduleDelay"></div>

        <p class="small">
          Vehicle ID: {{ $trip->vehicle_position->vehicle_id }}
        </p>
        <hr>
        <h5>Particle Information</h5>

        <dl class="dl-horizontal">
          <dt>Average Speed</dt>
          <dd id="particleSpeeds">&ndash;</dd>

          <dt>Average Delay</dt>
          <dd id="particleDelay">&ndash;</dd>

          <dt>Average Dwell Time</dt>
          <dd id="particleDwell">&ndash;</dd>
        </dl>
      </div>
    </div>
    <hr>

  </div>
@endsection

@section('endmatter')
  <script>
    var stops = {!! $stops->toJson() !!};

    function initMap() {

      var data = [
        @foreach ($shape as $point)
          { lat: {{ $point->lat }}, lng: {{ $point->lon }} },
        @endforeach
      ];

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

      var stopmarkers = [];

      for (var i = 0; i < stops.length; i++) {
        pos = new google.maps.LatLng(parseFloat(stops[i].stop.lat),
                                     parseFloat(stops[i].stop.lon));
        color = '{{ ($trip->route->color) ? $trip->route->color : "#000099" }}';
        stopmarkers[i] = new google.maps.Marker({
          position: pos,
          icon: {
            path: google.maps.SymbolPath.CIRCLE,
            scale: 3,
            strokeColor: color,
            fillColor: color,
            fillOpacity: 1
          },
          draggable: false,
          map: map,
          zIndex: 1
        });
      }

      var marker,
          time = {{ $trip->vehicle_position->timestamp }},
          particles = [],
          particleTime = 0;

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

            var tu = vehicle.trip_update;
            // console.log(tu);
            if (!$.isEmptyObject(tu)) {
              stu = tu.stop_time_updates;
              if (!$.isEmptyObject(stu)) {
                var delay = stu[0].arrival_delay + stu[0].departure_delay;
                var delayTxt = moment.duration(Math.abs(delay), 'seconds').format("m[m] s[s]");
                if (delay == 0) {
                  delayTxt = 'On schedule';
                } else if (delay > 0) {
                  delayTxt += ' ahead of schedule';
                } else {
                  delayTxt += ' behind schedule';
                }
                $("#scheduleDelay").html('<p>' + delayTxt + '</p>');
              }
            }

            // add the particles ... if they exist!
            if (vehicle.particles.length > 0) {
              $("#particleSpeeds").html(vehicle.speed);
              if (vehicle.particles[1].timestamp > particleTime) {
                if (particles.length > 0) {
                  for (var i = 0; i < particles.length; i++) {
                    particles[i].setMap(null);
                  }
                  particles = [];
                }
                var delaySum = 0,
                    dwellSum = 0,
                    dwellN   = 0;
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
                  delaySum += Math.max(vehicle.particles[i].arrival_time,
                                       vehicle.particles[i].departure_time) -
                    moment(moment().format('YYYY-MM-DD') + ' ' +
                    stops[vehicle.particles[i].segment - 1].arrival_time).unix();
                  dwellN += vehicle.particles[i].departure_time < vehicle.particles[i].arrival_time ? 0 : 1;
                  dwellSum += (vehicle.particles[i].departure_time < vehicle.particles[i].arrival_time) ? 0 :
                              vehicle.particles[i].departure_time - vehicle.particles[i].arrival_time;
                }
                var delay = delaySum / vehicle.particles.length;
                $("#particleDelay").html(moment.duration(Math.abs(delay), 'seconds').format("m[m] s[s]"));
                var dwell = dwellSum / dwellN;
                $("#particleDwell").html(moment.duration(Math.abs(dwell), 'seconds').format("m[m] s[s]"));
                particleTime = vehicle.particles[1].timestamp;
              }
            }
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
