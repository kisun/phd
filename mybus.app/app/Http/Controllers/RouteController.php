<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Route;

class RouteController extends Controller
{
    public function index()
    {
        $today = \Carbon\Carbon::now()->toDateString();
        $latest = \App\Version::where('startdate', '<=', $today)
                    ->where('enddate', '>=', $today)
                    ->orderBy('startdate', 'desc')->first();
        $routes = Route::where('version_id', $latest->id)->orderBy('short_name')->get();

        return view('routes.index', [
            'routes' => $routes
        ]);
    }

    public function show($route_id)
    {
        $today = \Carbon\Carbon::now()->toDateString();
        $latest = \App\Version::where('startdate', '<=', $today)
                    ->where('enddate', '>=', $today)
                    ->orderBy('startdate', 'desc')->first();
        $route = Route::where('route_id', $route_id)
                    ->where('version_id', $latest->id)
                    ->with(['trips' => function($query) {
                        $query->join('stop_times', 'stop_times.trip_id', 'trips.id')
                              ->join('calendars', 'trips.service_id', 'calendars.id')
                              ->select('trips.*', 'stop_times.stop_sequence', 'stop_times.departure_time',
                                       'calendars.*')
                              ->where('stop_times.stop_sequence', '=', '1')
                              ->orderBy('stop_times.departure_time');
                    }, 'trips.calendar'])
                    ->first();

        return view('routes.show', [
            'route' => $route,
            'shape' => $route->trips[0]->getShape(),
            'stops' => $route->trips[0]->stop_times()->with('stop')->get(),
            'dows' => ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
            'intersections' => \App\Intersection::all()
        ]);
    }
}
