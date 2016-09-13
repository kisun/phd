<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Trip;

class TripController extends Controller
{
    public function show($trip_id)
    {
        $latest = \App\Version::orderBy('startdate', 'desc')->first();
        $trip = Trip::where('trip_id', $trip_id)->where('version_id', $latest->id)->first();

        $shape = $trip->getShape();
        return view('map', [
          'trip' => $trip,
          'shape' => $shape
        ]);
    }
}
