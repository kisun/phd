@extends('layouts.app')

@section('content')
  <div class="page">
    <h3>{{ $route->short_name }} <small>{{ $route->long_name }}</small></h3>

    <hr>
    <p>
      @foreach (["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"] as $dow)
        <button type="button" class="btn btn-default"
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
    </p>

  </div>
@endsection


@section('endmatter')
  <script>
    new Vue({
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
    });
  </script>
@endsection
