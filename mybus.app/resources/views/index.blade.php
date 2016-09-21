@extends('layouts.app')

@section('content')
  {{--
    This is inside .app-content
   --}}

  <div class="page page-index">
    <div class="row text-center">
      <div class="col-sm-8 col-sm-offset-2 col-lg-6 col-lg-offset-3">
      <div id="userLocation" class="text-center">
        {{-- Only display if GPS position unavailable --}}
        <form action="{{ url('/search') }}" id="locationForm" method="post">
            <input type="text" class="form-control" placeholder="Starting Location">
        </form>
      </div>

        <h1>Where do you want to go?</h1>
        {{-- <form action="{{ url('/search') }}" method="post" id="searchForm"> --}}
        <form action="{{ url('/search') }}" method="get" id="searchForm">
          <div class="input-group input-group-lg">
            <input name = "routenumber" type="text" class="form-control" placeholder="Destination or Route #">
            <span class="input-group-btn">
              <button type="submit" class="btn btn-success">
                <span class="glyphicon glyphicon-search"></span>
              </button>
            </span>
          </div>
        </form>
      </div>
    </div>


    <div id="nearbyStops" class="row">
      <div class="col-sm-8 col-sm-offset-2 col-lg-6 col-lg-offset-3">
        <h4>Your closest stops:</h4>

        <table class="table table-striped table-hover">
          <thead>
            <tr>
              <th>ID</th>
              <th>Location</th>
            </tr>
          </thead>

          <tbody>
            <tr>
              <td>43256</td>
              <td>34 Symonds St</td>
            </tr>
            <tr>
              <td>1234</td>
              <td>38 Symonds St</td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>

    <div id="backgroundMap"></div>
    <div id="backgroundMapOverlay"></div>

  </div>

@endsection

@section('endmatter')
  <script>
    var map;

    function manualUserLocation() {
      $('#userLocation').addClass('visible');
    }

    function initMap() {
      var pos = {lat: -36.8485, lng: 174.7633};

      map = new google.maps.Map(document.getElementById('backgroundMap'), {
        center: pos,
        zoom: 15,
        disableDefaultUI: true
      });

      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(function(position) {
          pos = {
            lat: position.coords.latitude,
            lng: position.coords.longitude
          };

          map.setCenter(pos);

        }, function() {
          manualUserLocation();
        });
      } else {
        // Browser doesn't support Geolocation
        manualUserLocation();
      }

      google.maps.event.addListenerOnce(map, 'tilesloaded', function(){
        //loaded fully
        $(".app-content").addClass("opaque");
      });

      $("#locationForm").on('submit', function(e) {
        e.preventDefault();
        var location = $("#locationForm input").val();
        location = location.replace(/ /g, "+");
        $.get({
          url: "https://maps.googleapis.com/maps/api/geocode/json?address=" +
               location + "&region=nz&key={{ env('GOOGLE_API_KEY') }}",
          success: function(data) {
            var pos = data.results[0].geometry.location;
            var coord = new google.maps.LatLng(pos.lat, pos.lng);
            map.panTo(coord);
          }
        });
      });
    }

  </script>
  <script async defer
   src="https://maps.googleapis.com/maps/api/js?key={{ env('GOOGLE_API_KEY') }}&callback=initMap">
   </script>

@endsection
