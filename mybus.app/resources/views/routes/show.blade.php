@extends('layouts.app')

@section('content')
  <div class="page">
    <h3>{{ $route->short_name }} <small>{{ $route->long_name }}</small></h3>

    <hr>
    <div id="map" style="height:70vh"></div>
    <hr>
    <form action="{{ url('/api/intersections') }}" method="post" class="form-inline">
      <button class="btn btn-primary" id="addSegments">
        <span class="glyphicon glyphicon-plus"></span>
        Add Intersections</button>
      <select class="form-control" name="" id="">
        <option value="traffic_lights">Traffic Lights</option>
        <option value="pedestrian">Pedestrian Crossing</option>
        <option value="roundabout">Roundabout</option>
        <option value="uncontrolled">Other (uncontrolled)</option>
      </select>

      {{ csrf_field() }}
      <button class="btn btn-success" type="button" id="segmentRoute">Segment Route</button>
    </form>
  </div>
@endsection

@section('endmatter')
  <script>
    var map;

    function rad(x) {
      return x * Math.PI / 180;
    };

    function getDistance(p1, p2) {
      var R = 6378137; // Earth’s mean radius in meter
      var dLat = rad(p2.lat() - p1.lat());
      var dLong = rad(p2.lng() - p1.lng());
      var a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
        Math.cos(rad(p1.lat())) * Math.cos(rad(p2.lat())) *
        Math.sin(dLong / 2) * Math.sin(dLong / 2);
      var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
      var d = R * c;
      return d; // returns the distance in meters
    }

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

      map = new google.maps.Map(document.getElementById('map'), {
        center: pos,
        zoom: 12,
        disableDefaultUI: true,
        mapTypeId: 'hybrid'
      });
      map.setTilt(0);
      var shapePath = new google.maps.Polyline({
        path: data,
        geodesic: true,
        strokeColor: '{{ ($route->color) ? $route->color : "#000099" }}',
        strokeOpacity: 1.0,
        strokeWeight: 5
      });
      shapePath.setMap(map);

      var infoWindow = new google.maps.InfoWindow,
          stopmarkers = [],
          intersectionMarkers = [];

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

      var intersections = [
        @foreach ($intersections as $int)
          {
            id: {{ $int->id }},
            pos: new google.maps.LatLng({{ $int->lat }}, {{ $int->lon }}),
            type: "{{ $int->type }}",
          },
        @endforeach
      ];
      for (var i = 0; i < intersections.length; i++) {
        var lbl;
        switch (intersections[i].type) {
          case 'traffic_lights':
            lbl = 'A';
            break;
          case 'pedestrian':
            lbl = 'B';
            break;
          case 'roundabout':
            lbl = 'C';
            break;
          case 'uncontrolled':
            lbl = 'D';
        }
        intersectionMarkers[i] = new google.maps.Marker({
          position: intersections[i].pos,
          map: map,
          label: lbl,
          info: {
            id: intersections[i].id,
            type: intersections[i].type
          }
        });
        google.maps.event.addListener(intersectionMarkers[i], 'click', function() {
          var clicked = this;
          $.ajax({
            url: "{{ url('/api/intersections') }}/" + this.info.id,
            type: 'PUT',
            data: {
              type: $("form select").val()
            },
            success: function(result) {
              var lbl;
              switch (result.type) {
                case 'traffic_lights':
                  lbl = 'A';
                  break;
                case 'pedestrian':
                  lbl = 'B';
                  break;
                case 'roundabout':
                  lbl = 'C';
                  break;
                case 'uncontrolled':
                  lbl = 'D';
              }
              clicked.setLabel(lbl);
            }
          })
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
                map: map,
                info: vehicle,
                optimized: false,
              });
              google.maps.event.addListener(markers[i], 'click', infopanel);
            }
            if (set) {
              // map.fitBounds(bounds);
            }
          }
        });
      }
      updateMap(true);
      // setInterval(updateMap, 10000);

      var shape_click;
      function addIntersections() {
        // var direct = new google.maps.DirectionsService();
        $("#addSegments").html('<span class="glyphicon glyphicon-ok"></span> Done')
          .removeClass('btn-primary').addClass('btn-success');

        var count = 0;
        shape_click = shapePath.addListener('click', function(e) {
          $("form").append('<input type="hidden" name="intersections['+count+'][lat]" value='
                           + e.latLng.lat() + '>');
          $("form").append('<input type="hidden" name="intersections['+count+'][lon]" value='
                           + e.latLng.lng() + '>');
          $("form").append('<input type="hidden" name="intersections['+count+'][type]" value='
                           + $("form select").val() + '>');
          count++;

          new google.maps.Marker({
            position: e.latLng,
            map: map
          });
        });
      }

      $("#addSegments").on('click', function(e) {
        e.preventDefault();
        if ($(this).hasClass('btn-success')) {
          $("form").submit();
        } else {
          addIntersections();
        }
      });

      $("#segmentRoute").on('click', function(e) {
        e.preventDefault();
        // Do segmentation:
        var direct = new google.maps.DirectionsService();

        var Ns = intersections.length;

        // only want intersections "on" the route:
        for (var i = 0; i < Ns; i++) {
          var dist = 1000;
          for (var j = 0; j < data.length; j++) {
            d = getDistance(new google.maps.LatLng(parseFloat(data[j].lat), parseFloat(data[j].lng)),
                            intersections[i].pos);
            dist = Math.min(dist, d);
          }
          if (dist > 50) {
            intersectionMarkers[i].setMap(null);
          }
        }

        // var wps = [];
        // if (Ns <= 10) {
        //   for (i = 1; i < Ns - 1; i++) {
        //     wps.push(stops[i].stop.lat + ',' + stops[i].stop.lon);
        //   }
        // } else {
        //   for (i = 1; i <= 8; i++) {
        //     si = Math.round((Ns - 2) / 8 * i);
        //     wps.push({
        //       location: new google.maps.LatLng(parseFloat(stops[si].stop.lat),
        //                                        parseFloat(stops[si].stop.lon)),
        //       stopover: true
        //     });
        //   }
        // }
        // direct.route({
        //   origin: data[0],
        //   destination: data[data.length - 1],
        //   travelMode: 'DRIVING',
        //   waypoints: wps
        // }, function(result, status) {
        //   if (status == "OK") {
        //     console.log(result);
        //     // var geowps = result.geocoded_waypoints;
        //     var display = new google.maps.DirectionsRenderer();
        //     display.setMap(map);
        //     display.setDirections(result);
        //   }
        // });
      });
    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
