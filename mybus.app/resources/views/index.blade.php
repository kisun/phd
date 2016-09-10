@extends('layouts.app')

@section('content')
  {{--
    This is inside .app-content
   --}}

  <div class="page page-index">
    <div id="userLocation">
      {{-- Only display if GPS position unavailable --}}
      <form action="{{ url('/search') }}" id="locationForm" method="post">
          <input type="text" class="form-control" placeholder="Starting Location">
      </form>
    </div>

    <h1>Where do you want to go?</h1>

    <form action="{{ url('/search') }}" method="post" id="searchForm">
      <div class="input-group input-group-lg">
        <input type="text" class="form-control" placeholder="Destination or Route #">
        <span class="input-group-btn">
          <button type="submit" class="btn btn-success">
            <span class="glyphicon glyphicon-search"></span>
          </button>
        </span>
      </div>
    </form>

    <div id="backgroundMap"></div>
    <div id="backgroundMapOverlay"></div>

  </div>

@endsection

@section('endmatter')
  <script>
    function manualUserLocation() {
      $('#userLocation').addClass('visible');
    }

    function initMap() {
      if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(function(position) {
            var pos = {
              lat: position.coords.latitude,
              lng: position.coords.longitude
            };

            var map = new google.maps.Map(document.getElementById('backgroundMap'), {
              center: pos,
              zoom: 15,
              disableDefaultUI: true
            });

            $(".app-content").addClass("opaque");

            // $("#userLocation").html(pos['lat'] + ', ' + pos['lng']);
            // $("#userLocation").addClass('visible');

            // infoWindow.setPosition(pos);
            // infoWindow.setContent('Location found.');
            // map.setCenter(pos);
          }, function() {
            manualUserLocation();
          });
        } else {
          // Browser doesn't support Geolocation
          manualUserLocation();
        }
    }
  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>
@endsection
