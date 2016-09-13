<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Route;

class RouteController extends Controller
{
    public function index()
    {
        $latest = \App\Version::orderBy('startdate', 'desc')->first();
        $routes = Route::where('version_id', $latest->id)->orderBy('short_name')->get();

        echo '<ul>';
        foreach ($routes as $route) {
          echo '<li><a href="' . url('/routes/' . $route->route_id) . '">' .
               $route->short_name . ' - ' . $route->long_name . '</a></li>';
        }
        echo '</ul>';
    }

    public function show($route_id)
    {
        $latest = \App\Version::orderBy('startdate', 'desc')->first();
        $route = Route::where('route_id', $route_id)
                    ->where('version_id', $latest->id)
                    ->with(['trips' => function($query) {
                        $query->join('stop_times', 'stop_times.trip_id', 'trips.id')
                              ->select('trips.*', 'stop_times.stop_sequence', 'stop_times.departure_time')
                              ->where('stop_times.stop_sequence', '=', '1')
                              ->orderBy('stop_times.departure_time');
                    }, 'trips.calendar'])
                    ->first();

        $trips = $route->trips;

        echo '<h3>' . $route->short_name . ' - ' . $route->long_name . '</h3>';

        echo '<ul class="list-group">';
        foreach ($trips as $trip) {
          echo '<li class="list-group-item"><a href="' . url('/trips/' . $trip->trip_id) . '">' .
               $trip->start_time()->format('g:i a') . '</a> - ';
          $cal = $trip->calendar->toArray();
          foreach (['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as $day) {
            echo ($cal[$day] ? $day : '') . ' ';
          }
          echo '</li>';
        }
        echo '</ul>';
    }
}
