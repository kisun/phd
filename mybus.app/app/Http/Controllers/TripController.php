<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Trip;

class TripController extends Controller
{
    public function show($trip_id)
    {
        $today = \Carbon\Carbon::now()->toDateString();
        $latest = \App\Version::where('startdate', '<=', $today)
                    ->where('enddate', '>=', $today)
                    ->orderBy('startdate', 'desc')->first();
        $trip = Trip::where('trip_id', $trip_id)
                    ->where('version_id', $latest->id)
                    ->with('vehicle_position.particles')
                    ->first();

        $shape = $trip->getShape();
        return view('trips.show', [
          'trip' => $trip,
          'shape' => $shape,
          'stops' => $trip->stop_times()->with('stop')->get()
        ]);
    }
}
