<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;
use App\Intersection;

class IntersectionController extends Controller
{
    public function create(Request $request)
    {
        $intersections = $request->intersections;
        foreach($intersections as $intersection) {
            $int = new Intersection;
            $int->lat = $intersection['lat'];
            $int->lon = $intersection['lon'];
            $int->type = $intersection['type'];
            $int->save();
        }
    }
}
