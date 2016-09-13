@extends('layouts.app')

@section('content')
  <div class="page">
    <h3>{{ $route->short_name }} <small>{{ $route->long_name }}</small></h3>


    @foreach (["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"] as $dow)
      <h5>{{ $dow }}</h5>

      <p>
        @foreach ($route->trips as $trip)
          @if ($trip->calendar->toArray()[strtolower($dow)])
            <a href="{{ url('/trips/' . $trip->trip_id) }}" class="btn btn-link">
              {{ $trip->start_time()->format('g:i a') }}
            </a>
          @endif
        @endforeach
      </p>
    @endforeach
  </div>
@endsection
