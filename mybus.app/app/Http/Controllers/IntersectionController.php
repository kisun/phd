<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;
use App\Intersection;

class IntersectionController extends Controller
{
    public function create(Request $request)
    {
        if (is_null($request->intersections)) {
            return back();
        }
        $intersections = $request->intersections;
        foreach($intersections as $intersection) {
            $int = new Intersection;
            $int->lat = $intersection['lat'];
            $int->lon = $intersection['lon'];
            $int->type = $intersection['type'];
            $int->save();
        }
        return back();
    }

    public function update(Request $request, Intersection $intersection)
    {
        $intersection->type = $request->type;
        $intersection->save();
        return response()->json($intersection);
    }
}
