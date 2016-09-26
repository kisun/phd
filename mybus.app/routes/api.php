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
