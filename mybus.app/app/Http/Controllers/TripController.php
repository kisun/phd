<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Trip;

class TripController extends Controller
{
    public function show(Trip $trip)
    {
        $shape = $trip->shapes;
        return view('map', [
          'shape' => $shape
        ]);
    }
}
