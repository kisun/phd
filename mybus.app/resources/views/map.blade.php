@extends('layouts.app')


@section('content')
  <div class="page">
    <h3>
      {{ $trip->route->short_name }}
      <small>{{ $trip->route->long_name }}</small>
    </h3>
    <div id="shapeMap"></div>
  </div>
@endsection

@section('endmatter')
  <script>
    function manualUserLocation() {
      $('#userLocation').addClass('visible');
    }

    function initMap() {

      var data = [
        @foreach ($shape as $point)
          { lat: {{ $point->lat }}, lng: {{ $point->lon }} },
        @endforeach
      ];

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

      var map = new google.maps.Map(document.getElementById('shapeMap'), {
        center: pos,
        zoom: 12,
        // disableDefaultUI: true
      });
      var shapePath = new google.maps.Polyline({
        path: data,
        geodesic: true,
        strokeColor: '{{ ($trip->route->color) ? $trip->route->color : "#000099" }}',
        strokeOpacity: 1.0,
        strokeWeight: 4
      });

      shapePath.setMap(map);

    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
