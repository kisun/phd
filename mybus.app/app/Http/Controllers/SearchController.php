<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;
use App\Route;

class SearchController extends Controller
{
    public function search(Request $request)
    {
        $today = \Carbon\Carbon::now()->toDateString();
        $latest = \App\Version::where('startdate', '<=', $today)
                    ->where('enddate', '>=', $today)
                    ->orderBy('startdate', 'desc')->first();
        // only route number supported
        if (isset($request->routenumber)) {
            $routes = Route::where('route_id', 'ILIKE', $request->routenumber . '%')
                          ->where('version_id', $latest->id)->get();
            return view('routes.index', [
              'routes' => $routes,
              'search' => $request->routenumber
            ]);
        }
    }
}
