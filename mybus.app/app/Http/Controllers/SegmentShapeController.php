<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;
use App\Route;
use App\SegmentInfo;
use App\SegmentShape;
use App\Segment;

class SegmentShapeController extends Controller
{
    public function create(Request $request, Route $route)
    {
        // DELETE old shapes:
        foreach ($route->trips as $trip) {
            SegmentShape::where('id', $trip->shape_id)->delete();
        }

        // create the segments
        for ($j=0; $j<count($request->legs)-1;$j++) {
            $leg = $request->legs[$j];
            if (!is_numeric($leg[0]['intersection_id'])) {
                $segment = SegmentInfo::where('end_id', $leg[count($leg)-1]['intersection_id'])
                                    ->whereNull('start_id')->first();
                if (count($segment) == 0) {
                    $segment = new SegmentInfo;
                    $segment->end_id = $leg[count($leg)-1]['intersection_id'];
                }
            } else if (!is_numeric($leg[count($leg) - 1]['intersection_id'])) {
                $segment = SegmentInfo::where('stard_id', $leg[0]['intersection_id'])
                                    ->whereNull('end_id')->first();
                if (count($segment) == 0) {
                    $segment = new SegmentInfo;
                    $segment->start_id = $leg[0]['intersection_id'];
                }
            } else {
                $segment = SegmentInfo::firstOrNew([
                    'start_id' => $leg[0]['intersection_id'],
                    'end_id' => $leg[count($leg)-1]['intersection_id']
                ]);
            }
            if (is_null($segment->id)) {
                // its new: add length!
                $segment->length = $leg[count($leg) - 1]['dist'] - $leg[0]['dist'];
                $segment->save();
                for ($i = 0; $i < count($leg); $i++) {
                    Segment::create([
                        'segment_id' => $segment->id,
                        'lat' => $leg[$i]['lat'],
                        'lon' => $leg[$i]['lon'],
                        'pt_sequence' => $i + 1,
                        'dist_traveled' => $leg[$i]['dist'] - $leg[0]['dist']
                    ]);
                }
            }

            foreach ($route->trips as $trip) {
                $shapeInfo = $trip->getShape()[0];
                $shape = SegmentShape::create([
                    'id' => $shapeInfo->id,
                    'shape_id' => $shapeInfo->shape_id,
                    'leg' => $j + 1,
                    'segment_id' => $segment->id,
                    'dist_traveled' => $leg[0]['dist'],
                    'version_id' => $shapeInfo->version_id
                ]);
            }
        }
        return response()->json([
            'segment_infos' => SegmentInfo::all(),
            'segments' => Segment::all(),
            'shape_segment' => SegmentShape::all()
        ]);
    }
}
