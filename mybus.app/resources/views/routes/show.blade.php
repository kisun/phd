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
