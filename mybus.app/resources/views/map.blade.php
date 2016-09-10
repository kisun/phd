@extends('layouts.app')


@section('content')
  <div class="page">
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
          { lat: {{ $point->shape_pt_lat }}, lng: {{ $point->shape_pt_lon }} },
        @endforeach
      ];
      console.log(data);

      var pos = {
        lat: (
          @foreach($shape as $point)
            {{ $point->shape_pt_lat }} +
          @endforeach
          + 0) / {{ count($shape) }},
        lng: (
          @foreach($shape as $point)
            {{ $point->shape_pt_lon }} +
          @endforeach
          + 0) / {{ count($shape) }}
      };

      var map = new google.maps.Map(document.getElementById('shapeMap'), {
        center: pos,
        zoom: 11,
        disableDefaultUI: true
      });
      var shapePath = new google.maps.Polyline({
        path: data,
        geodesic: true,
        strokeColor: '#000099',
        strokeOpacity: 1.0,
        strokeWeight: 2
      });

      shapePath.setMap(map);

    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
