@extends('layouts.app')

@section('content')
  <div class="page">
    @if (isset($search))
      <h3>Routes matching your search for "{{ $search }}"</h3>
    @endif

    <ul class="list-group">
      @foreach ($routes as $route)
        <a class="list-group-item" href="{{ url('/routes/' . $route->route_id) }}">
          {{ $route->short_name }} - {{ $route->long_name }}
        </a>
      @endforeach
    </ul>
  </div>
@endsection
