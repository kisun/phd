<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;
use App\Route;
use App\ShapeSegment;
use App\Segment;

class SegmentShapeController extends Controller
{
    public function create(Request $request, Route $route)
    {
        // create the segments
        foreach ($request->legs as $leg) {
            $segmentID = Segment::max('id');
            if (is_null($segmentID)) {
                $segmentID = 0;
            } else {
                $segmentID += 1;
            }
            for ($i = 0; $i < count($leg); $i++) {
                $segment = Segment::create([
                    'id' => $segmentID,
                    'lat' => $leg[$i]['lat'],
                    'lon' => $leg[$i]['lon'],
                    'pt_sequence' => $i + 1,
                    'dist_traveled' => $leg[$i]['dist']
                ]);
            }
        }
        return response()->json(Segment::all());
    }
}
