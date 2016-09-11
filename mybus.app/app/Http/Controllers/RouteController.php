<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Route;

class RouteController extends Controller
{
    public function index()
    {
        $routes = Route::where('route_id', 'LIKE', '%' . "20160907091452_v45.21")
                    ->orderBy('route_short_name')->get();

        echo '<ul>';
        foreach ($routes as $route) {
          echo '<li><a href="' . url('/routes/' . $route->route_id) . '">' .
               $route->route_short_name . ' - ' . $route->route_long_name . '</a></li>';
        }
        echo '</ul>';
    }

    public function show(Route $route)
    {
        $route->load(['trips' => function($query) {
            $query->join('stop_times', 'stop_times.trip_id', 'trips.trip_id')
                  ->select('trips.*', 'stop_times.stop_sequence', 'stop_times.departure_time')
                  ->where('stop_times.stop_sequence', '=', '1')
                  ->orderBy('stop_times.departure_time');
        }, 'trips.calendar']);

        $trips = $route->trips;

        echo '<h3>' . $route->route_short_name . ' - ' . $route->route_long_name . '</h3>';

        echo '<ul>';
        foreach ($trips as $trip) {
          echo '<li><a href="' . url('/trips/' . $trip->trip_id) . '">' .
               $trip->start_time() . '</a> - ';
          $cal = $trip->calendar->toArray();
          foreach (['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as $day) {
            echo ($cal[$day] ? $day : '') . ' ';
          }
          echo '</li>';
        }
        echo '</ul>';
    }
}
