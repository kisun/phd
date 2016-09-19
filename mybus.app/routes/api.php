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
  return response()->json($route->vehicle_positions()->with('trip.stop_times')->get());
});


Route::get('/shape_schedule/{trip}', function(App\Trip $trip) {
  return response()->json([
    'shape' => $trip->getShape(),
    'schedule' => $trip->stops
  ]);
});
