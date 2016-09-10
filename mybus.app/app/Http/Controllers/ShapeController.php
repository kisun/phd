<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;

use App\Shape;

class ShapeController extends Controller
{
    public function show($id)
    {
        $shape = Shape::where('shape_id', $id)->orderBy('shape_pt_sequence')->get();
        return view('map', [
          'shape' => $shape
        ]);
    }
}
