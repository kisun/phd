<?php

use Illuminate\Http\Request;

/*
|--------------------------------------------------------------------------
| API Routes
|--------------------------------------------------------------------------
|
| Here is where you can register API routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| is assigned the "api" middleware group. Enjoy building your API!
|
*/

Route::get('/user', function (Request $request) {
    return $request->user();
})->middleware('auth:api');


Route::get('/vehicle_positions/{route}', function(App\Route $route) {
  return response()->json($route->vehicle_positions()->with('trip.stop_times')->get(),
                          200, [], JSON_NUMERIC_CHECK);
});
Route::get('/vehicle_position/{trip}', function(App\Trip $trip) {
  $obj = $trip->vehicle_position()->with('particles')->first();
  $obj->trip_update = $obj->getTripUpdate();
  $obj->vehicle_id = '"' . $obj->vehicle_id . '"';
  $obj->vehicle_label = '"' . $obj->vehicle_label . '"';

  $speed = $trip->vehicle_position->particles()
            ->select(DB::raw('avg(velocity) as mean, stddev(velocity) as sd'))->first();
  $obj->speed = round($speed->mean, 2) . "m/s (&plusmn; " . round($speed->sd, 2) . ")";

  return response()->json($obj, 200, [], JSON_NUMERIC_CHECK | JSON_UNESCAPED_SLASHES);
});


Route::get('/shape_schedule/{trip}', function(App\Trip $trip) {
    return response()->json([
        'shape' => $trip->getShape(),
        'schedule' => $trip->stops
    ], 200, [], JSON_NUMERIC_CHECK);
});

Route::put('/stop_times/{trip}/{stop_sequence}', function(Request $request, App\Trip $trip, $stop_sequence) {
    $route = $trip->route;
    if (!is_null($request->shape_dist_traveled)) {
        $qry = "UPDATE stop_times SET shape_dist_traveled = " . $request->shape_dist_traveled .
                      " WHERE trip_id IN (SELECT id FROM trips WHERE route_id IN " .
                      " (SELECT id FROM routes WHERE route_id='" . $route->route_id .
                      "')) AND stop_sequence=" . $stop_sequence;
        DB::statement($qry);
        return response()->json('OK');
    }
});


Route::get('/delays', function() {
    return response()->json(
        \App\StopTimeUpdate::select(DB::raw('ROUND(AVG(arrival_delay + departure_delay), 2) AS delay'))
            ->join('stops', 'stops.id', 'stop_time_updates.stop_id')
            ->addSelect('stops.*')
            ->groupBy('stops.id')
            ->get()
    , 200, [], JSON_NUMERIC_CHECK);
});

Route::get('/segment_speeds/{trip}', function(App\Trip $trip) {
    return response()->json($trip->load(['segments' => function ($query) {
        $query->orderBy('leg')->with('segment_info');
    }]), 200, [], JSON_NUMERIC_CHECK);
});

Route::post('/intersections', 'IntersectionController@create');
Route::put('/intersections/{intersection}', 'IntersectionController@update');

Route::post('/route_shapes/{route_id}', 'SegmentShapeController@create');
Route::delete('/route_shapes/{route_id}', 'SegmentShapeController@destroy');
