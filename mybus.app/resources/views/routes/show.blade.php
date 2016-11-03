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
        <option value="DELETE">Delete</option>
      </select>

      {{ csrf_field() }}
      <button class="btn btn-success" type="button" id="segmentRoute">Segment Route</button>
    </form>
  </div>
@endsection

@section('endmatter')
  <script src="http://cdn.rawgit.com/chrisveness/geodesy/v1.1.1/latlon-spherical.js"></script>
  <script>
    var map;

    var R = 6371e3; // Earth’s mean radius in meter
    // function rad(x) {
    //   return x * Math.PI / 180;
    // };
    // function deg(x) {
    //   return x * 180 / Math.PI;
    // }
    //
    // function getDistance(p1, p2) {
    //   var dLat = rad(p2.lat() - p1.lat());
    //   var dLong = rad(p2.lng() - p1.lng());
    //   var a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    //     Math.cos(rad(p1.lat())) * Math.cos(rad(p2.lat())) *
    //     Math.sin(dLong / 2) * Math.sin(dLong / 2);
    //   var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    //   var d = R * c;
    //   return d; // returns the distance in meters
    // }
    // function getBearing(p1, p2) {
    //   var y = Math.sin(rad(p2.lng()) - rad(p1.lng())) * Math.cos(rad(p2.lat()));
    //   var x = Math.cos(rad(p1.lat())) * Math.sin(rad(p2.lat())) -
    //             Math.sin(rad(p1.lat())) * Math.cos(rad(p2.lat())) * Math.cos(rad(p2.lng()) - rad(p1.lng()));
    //   var brng = deg(Math.atan2(y, x));
    //
    //   return (brng + 360) % 360;
    // }
    // function getDestination(q1, theta, d) {
    //   // q1 is a lat/lng
    //   var phi1 = rad(q1.lat()),
    //       lam1 = rad(q1.lng());
    //   var phi = Math.asin(Math.sin(phi1) * Math.cos(d / R) +
    //                       Math.cos(phi1) * Math.sin(d / R) * Math.cos(theta));
    //   var lam = lam1 + Math.atan2(Math.sin(theta) * Math.sin(d / R) * Math.cos(phi1),
    //                               Math.cos(d / R) - Math.sin(phi1) * Math.sin(phi));
    //   return new google.maps.LatLng(deg(phi), deg(lam));
    // }

    function initMap() {

      var data = [
        @foreach ($shape as $point)
          { lat: {{ $point->lat }}, lng: {{ $point->lon }}, dist: {{ $point->dist_traveled }} },
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

      // regenerate the shape, nicer!
      // Convert AT shape to Google Snap-to-roads:
      var pathValues = [];
      for (var i = 0; i < data.length; i++) {
        pathValues.push(data[i].lat + ',' + data[i].lng);
      }
      // every 100 obs:
      var snappedPath = [];
      var M = Math.ceil(data.length / 99);
      for (var i = 0; i < M; i++) {
        // overlap: 0-99; 99-198; 198-297; ...
        $.ajax({
          url: 'https://roads.googleapis.com/v1/snapToRoads',
          data: {
            interpolate: true,
            key: "{{ env('GOOGLE_API_KEY') }}",
            path: pathValues.slice(99 * i, 99 * (i + 1) + 1).join('|')
          },
          success: function(data) {
            for (j = 0; j < data.snappedPoints.length; j++) {
              snappedPath.push({lat: data.snappedPoints[j].location.latitude,
                                lng: data.snappedPoints[j].location.longitude});
            }
          },
          async: false
        });
      }
      var data = [], cdist = 0;
      for (var i = 0; i < snappedPath.length; i++) {
        data[i] = {lat: snappedPath[i].lat, lng: snappedPath[i].lng, dist: cdist};
        if (i < snappedPath.length - 1) {
          var p1 = new LatLon(snappedPath[i].lat, snappedPath[i].lng);
          var p2 = new LatLon(snappedPath[i+1].lat, snappedPath[i+1].lng);
          cdist += p1.distanceTo(p2);
        }
      }

      map = new google.maps.Map(document.getElementById('map'), {
        center: pos,
        zoom: 12,
        disableDefaultUI: true,
        mapTypeControl: true
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

      // for (var i = 0; i < stops.length; i++) {
      //   pos = new google.maps.LatLng(parseFloat(stops[i].stop.lat),
      //                                parseFloat(stops[i].stop.lon));
      //   stopmarkers[i] = new google.maps.Marker({
      //     position: pos,
      //     icon: {
      //       path: google.maps.SymbolPath.CIRCLE,
      //       scale: 4
      //     },
      //     draggable: false,
      //     map: map,
      //     zIndex: 1
      //   });
      // }

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
              if (result == 'DELETED') {
                clicked.setMap(null);
              }

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
        shape_click = map.addListener('click', function(e) {
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
        var Ns = intersections.length;
        var Data = [];
        for (j = 0; j < snappedPath.length; j++) {
          Data[j] = new google.maps.LatLng(parseFloat(data[j].lat), parseFloat(data[j].lng));
        }

        // only want intersections "on" the route - do that later ...
        for (var i = 0; i < Ns; i++) {
          var dist = 1000;
          var p = new LatLon(intersections[i].pos.lat(), intersections[i].pos.lng());
          var dit;
          var wj = 0;
          for (var j = 0; j < data.length - 1; j++) {
            if (j < data.length) {
              if (Data[j].lat() == Data[j+1].lat() &
                  Data[j].lng() == Data[j+1].lng()) {
                continue;
              }
            }
            var q1 = new LatLon(Data[j].lat(), Data[j].lng());
            var q2 = new LatLon(Data[j + 1].lat(), Data[j + 1].lng());

            var Delta1 = q1.bearingTo(p) - q1.bearingTo(q2);
            var Delta2 = q2.bearingTo(p) - q2.bearingTo(q1);

            var r = 1000, d = 0, dx;
            if (Math.abs(Delta1) < 90 & Math.abs(Delta2) < 90) {
              // between q1 and q2
              r = p.crossTrackDistanceTo(q1, q2);
              d = Math.acos(Math.cos(q1.distanceTo(p) / R) / Math.cos(Math.abs(r) / R)) * R;
              dx = data[j].dist + d;

              // console.log("(" + j + ":" + data[j].dist + ") between ... " + r + " -> " + d);
            } else if (Math.abs(Delta1) >= 90 & Math.abs(Delta2) < 90) {
              // before q1
              r = q1.distanceTo(p);
              dx = data[j].dist;

              // console.log("(" + j + ":" + data[j].dist + ") before ..." + r);
            } else {
              // after q2
              r = q2.distanceTo(p);
              dx = data[j+1].dist;

              // console.log("(" + j + ":" + data[j].dist + ") after ..." + r);
            }

            if (Math.abs(r) <= dist) {
              dist = Math.abs(r);
              dit = dx;
              wj = j;
            }
          }
          if (Math.abs(dist) > 20) {
            intersectionMarkers[i].setMap(null);
          } else {
            intersections[i].dist = dit;
            intersections[i].segment = wj;
            intersections[i].resid = dist;
          }
        }

        var wps = [];
        for (var i = 0; i < intersections.length; i++) {
          if ("dist" in intersections[i]) {
            wps.push(intersections[i]);
          }
        }

        wps.sort(function(a, b) {
          return a.dist - b.dist;
        });


        var Ns = wps.length;
        var legs = [[]];
        var j = 0;
        for (var i=0; i<intersectionMarkers.length; i++) {
          intersectionMarkers[i].setMap(null);
        }
        // intersectionMarkers = [];

        for (var i=0; i<data.length-1; i++) {
          legs[j].push({
            lat: data[i].lat, lon: data[i].lng, dist: data[i].dist, intersection_id: ''
          });
          if (j == Ns) {
            continue;
          }
          if (wps[j].dist < data[i+1].dist) {
            // if the intersection is along the next segment:
            var q1 = new LatLon(Data[i].lat(), Data[i].lng()),
                q2 = new LatLon(Data[i+1].lat(), Data[i+1].lng()),
                d = wps[j].dist - data[i].dist;

            // we generate a point on the line:
            var pt = q1.destinationPoint(d, q1.bearingTo(q2));
            // intersectionMarkers.push(new google.maps.Marker({
            //   position: {lat: pt.lat, lng: pt.lon},
            //   map: map
            // }));
            legs[j].push({
              lat: pt.lat, lon: pt.lon, dist: wps[j].dist, intersection_id: wps[j].id
            });
            j++;
            legs.push([]);
            legs[j].push({
              lat: pt.lat, lon: pt.lon, dist: wps[j-1].dist, intersection_id: wps[j-1].id
            });
          }

        }


        shapePath.setMap(null);
        var legPaths = [];
        for (var i=0;i<legs.length;i++) {
          var dat = [];
          for (var j=0;j<legs[i].length;j++) {
            dat[j] = {lat: legs[i][j].lat, lng: legs[i][j].lon};
          }
          legPaths[i] = new google.maps.Polyline({
            path: dat,
            geodesic: true,
            strokeColor: (i % 2 == 0) ? "#990000" : "#000099",
            strokeOpacity: 1.0,
            strokeWeight: 5
          });
          legPaths[i].setMap(map);
        }

        // Delete old shapes:
        $.ajax({
          url: "{{ url('/api/route_shapes/' . $route->id) }}",
          type: "DELETE",
          async: false
        });
        // Create each LEG
        for (var i = 0; i < legs.length; i++) {
          $.ajax({
            url: "{{ url('/api/route_shapes/' . $route->id) }}",
            type: "POST",
            data: {
              leg: legs[i],
              seq: i
            },
            success: function(response) {
              console.log(response);
            }
          });
        }
      });
    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
