@extends('layouts.app')

@section('content')
  <div class="page">
    <h3>{{ $route->short_name }} <small>{{ $route->long_name }}</small></h3>

    <hr>
    <div id="map" style="height:70vh"></div>
    <hr>

    {{-- <p>
      @foreach (["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"] as $dow)
        <button type="button" class="btn btn-default btn-xs"
                v-bind:class="{ 'btn-success': dow == '{{ $dow }}' }"
                v-on:click="changeDay('{{ $dow }}')">
          {{ $dow }}
        </button>
      @endforeach
    </p>

    <p>
      <a v-for="trip in trips" href="@{{ trip.url }}" class="btn btn-link"
         v-if="(dow == 'Monday' && trip.monday) ||
               (dow == 'Tuesday' && trip.tuesday) ||
               (dow == 'Wednesday' && trip.wednesday) ||
               (dow == 'Thursday' && trip.thursday) ||
               (dow == 'Friday' && trip.friday) ||
               (dow == 'Saturday' && trip.saturday) ||
               (dow == 'Sunday' && trip.sunday)">
        @{{ trip.time }}
      </a>
    </p> --}}
  </div>
@endsection

@section('endmatter')
  <script>
    {{-- new Vue({
      el: '.page',

      data: {
        dow: 'Monday',
        trips: [
          @foreach ($route->trips as $trip)
            {
              url: "{{ url('/trips/' . $trip->trip_id ) }}",
              time: "{{ \Carbon\Carbon::createFromFormat('H:i:s', $trip->departure_time)->format('g:i a') }}",
              monday: "{{ $trip->monday or 0 }}" == 1,
              tuesday: "{{ $trip->tuesday }}" == 1,
              wednesday: "{{ $trip->wednesday }}" == 1,
              thursday: "{{ $trip->thursday }}" == 1,
              friday: "{{ $trip->friday }}" == 1,
              saturday: "{{ $trip->saturday }}" == 1,
              sunday: "{{ $trip->sunday }}" == 1,
            },
          @endforeach
        ]
      },

      methods: {
        changeDay: function(day) {
          this.dow = day
        }
      }
    }); --}}

    function initMap() {

      var data = [
        @foreach ($shape as $point)
          { lat: {{ $point->lat }}, lng: {{ $point->lon }} },
        @endforeach
      ];

      // Convert AT shape to Google Snap-to-roads:
      // var pathValues = [];
      // for (var i = 0; i < data.length; i++) {
      //   pathValues.push(data[i].lat + ',' + data[i].lng);
      // }
      // every 100 obs:
      // var snappedPath = [];
      // var M = Math.ceil(data.length / 99);
      // for (var i = 0; i < M; i++) {
      //   // overlap: 0-99; 99-198; 198-297; ...
      //   $.ajax({
      //     url: 'https://roads.googleapis.com/v1/snapToRoads',
      //     data: {
      //       interpolate: true,
      //       key: "{{ env('GOOGLE_API_KEY') }}",
      //       path: pathValues.slice(99 * i, 99 * (i + 1) + 1).join('|')
      //     },
      //     success: function(data) {
      //       for (j = 0; j < data.snappedPoints.length; j++) {
      //         snappedPath.push({lat: data.snappedPoints[j].location.latitude,
      //                           lng: data.snappedPoints[j].location.longitude});
      //       }
      //     },
      //     async: false
      //   });
      // }

      // var csvContent = "data:text/csv;charset=utf-8,";
      // for (i = 0; i < snappedPath.length; i++) {
      //   csvContent += snappedPath[i].lat + ',' + snappedPath[i].lng + '\n';
      // }
      // var encodedUri = encodeURI(csvContent);
      // var link = document.createElement("a");
      // link.setAttribute("href", encodedUri);
      // link.setAttribute("download", "shape.csv");
      // document.body.appendChild(link);
      // link.click();

      var stops = {!! $stops->toJson() !!};

      var pos = {
        lat: (
          @foreach($shape as $point)
            {{ $point->lat }} +
          @endforeach
          + 0) / {{ count($shape) }},
        lng: (
          @foreach($shape as $point)
            {{ $point->lon }} +
          @endforeach
          + 0) / {{ count($shape) }}
      };

      var map = new google.maps.Map(document.getElementById('map'), {
        center: pos,
        zoom: 12,
        disableDefaultUI: true
      });
      var shapePath = new google.maps.Polyline({
        path: data,
        geodesic: true,
        strokeColor: '{{ ($route->color) ? $route->color : "#000099" }}',
        strokeOpacity: 1.0,
        strokeWeight: 2
      });
      shapePath.setMap(map);

      // var shapeSnapPath = new google.maps.Polyline({
      //   path: snappedPath,
      //   geodesic: true,
      //   strokeColor: "#990000",
      //   strokeOpacity: 1.0,
      //   strokeWeight: 2
      // });
      // shapeSnapPath.setMap(map);

      // Get "directions":
      // https://maps.googleapis.com/maps/api/directions/json?origin=-36.84447856002873,174.768216991521&destination=-36.9080104104803,174.75875468490406&key=AIzaSyD9mbTafx7_v5gY7F4JqmnqSz5mrZkFx2Y&waypoints=
      var direct = new google.maps.DirectionsService();
      // need 8 waypoints!
      var Ns = stops.length;
      var wps = [];
      if (Ns <= 10) {
        for (i = 1; i < Ns - 1; i++) {
          wps.push(stops[i].stop.lat + ',' + stops[i].stop.lon);
        }
      } else {
        for (i = 1; i <= 8; i++) {
          si = Math.round((Ns - 2) / 8 * i);
          wps.push({
            location: new google.maps.LatLng(parseFloat(stops[si].stop.lat),
                                             parseFloat(stops[si].stop.lon)),
            stopover: true
          });
        }
      }
      direct.route({
        origin: data[0],
        destination: data[data.length - 1],
        travelMode: 'DRIVING',
        waypoints: wps
      }, function(result, status) {
        if (status == "OK") {
          console.log(result);
          // var geowps = result.geocoded_waypoints;
          var display = new google.maps.DirectionsRenderer();
          display.setMap(map);
          display.setDirections(result);
        }
      });

      var infoWindow = new google.maps.InfoWindow,
          stopmarkers = [];

      for (var i = 0; i < stops.length; i++) {
        pos = new google.maps.LatLng(parseFloat(stops[i].stop.lat),
                                     parseFloat(stops[i].stop.lon));
        stopmarkers[i] = new google.maps.Marker({
          position: pos,
          icon: {
            path: google.maps.SymbolPath.CIRCLE,
            scale: 4
          },
          draggable: false,
          map: map,
          zIndex: 1
        });
      }

      var markers = [],
          infowindow = new google.maps.InfoWindow();

      function infopanel() {
        console.log(this.info);
        infowindow.close();
        infowindow.setContent(getContent(this.info));
        infowindow.open(map, this);
      }

      function getContent(vehicle) {
        var html = '';
        html += '<h4>{{ $route->short_name }}: {{ $route->long_name }}</h4>';
        html += '<p>Departs ' +
            vehicle.trip.stop_times[0].departure_time +
            '</p>';
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
        html += '<p><a class="btn btn-primary" href="{{ url("/trips") }}/' + vehicle.trip.trip_id
             + '">View More</a>';
        return html;
      }

      function updateMap(set = false) {
        if (markers.length > 0) {
          for (var i = 0; i < markers.length; i++) {
            markers[i].setMap(null);
          }
          markers = [];
        }
        $.get({
          url: '/api/vehicle_positions/{{ $route->id }}',
          success: function (data) {
            for (var i=0; i<data.length; i++) {
              vehicle = data[i];
              pos = new google.maps.LatLng(vehicle.position_latitude, vehicle.position_longitude);
              markers[i] = new google.maps.Marker({
                position: pos,
                //icon: "{{ url('img/bus.png') }}",
                map: map,
                info: vehicle,
                optimized: false,
                // zIndex: 5
              });
              google.maps.event.addListener(markers[i], 'click', infopanel);
              //google.maps.event.addListener(markers[i], 'mouseover', lastStop);
              //google.maps.event.addListener(markers[i], 'mouseout', clearStop);
              //bounds.extend(pos);
            }
            if (set) {
              // map.fitBounds(bounds);
            }
          }
        });
      }
      updateMap(true);
      setInterval(updateMap, 10000);
    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
