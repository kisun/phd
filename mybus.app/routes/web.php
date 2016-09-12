<?php

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| This file is where you may define all of the routes that are handled
| by your application. Just tell Laravel the URIs it should respond
| to using a Closure or controller method. Build something great!
|
*/

Route::get('/', function () {
    return view('index');
});

Auth::routes();
Route::get('/logout', function() {
  Auth::logout();
  return redirect('/')->with('status', 'We hope to see you soon!');
});

Route::get('/home', 'HomeController@index');


Route::get('/routes', 'RouteController@index');
Route::get('/routes/{route}', 'RouteController@show');

Route::get('/trips/{trip}', 'TripController@show');
Route::get('/shapes/{id}', 'ShapeController@show');



/* testing */
Route::get('/update', function() {
    // Current versions:
    // $versions = App\Version::orderBy('startdate', 'desc')->get();
    //
    // $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/versions');
    // $url = $req->getUrl();
    // $req->setHeader([
    //     'Ocp-Apim-Subscription-Key' => env('AT_API_KEY'),
    // ]);
    //
    // $req->setMethod(HTTP_Request2::METHOD_GET);
    // try {
    //     $response = $req->send();
    //     $new_versions = json_decode($response->getBody(), true)['response'];
    // } catch (HttpException $ex) {
    //     echo $ex;
    //     dd();
    // }
    //
    // // Check if each one is already in the data base :
    // foreach ($new_versions as $new_version) {
    //   if (! $versions->contains('version', $new_version['version'])) {
    //       $version = new App\Version;
    //       $version->create($new_version);
    //   }
    //
    //       $version = App\Version::find($new_version['version']);
    //
    //       // and then we need to do ALL the work!
    //
    //       // -- AGENCIES
    //       $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/agency');
    //       $req->setHeader([
    //           'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       ])->setMethod(HTTP_Request2::METHOD_GET);
    //
    //       try {
    //           $response = $req->send();
    //           $agencies = json_decode($response->getBody())->response;
    //       } catch (HttpException $ex) {
    //           echo $ex;
    //           dd();
    //       }
    //
    //       foreach ($agencies as $a) {
    //           $agency = App\Agency::where('agency_id', $a->agency_id)->firstOrCreate([
    //               'agency_id' => $a->agency_id,
    //               'name' => $a->agency_name,
    //               'url' => $a->agency_url,
    //               'timezone' => $a->agency_timezone,
    //               'lang' => $a->agency_lang,
    //               'phone' => $a->agency_phone
    //           ]);
    //       }
    //
    //       // -- ROUTES
    //       $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/routes');
    //       $req->setHeader([
    //           'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       ])->setMethod(HTTP_Request2::METHOD_GET);
    //
    //       try {
    //           $response = $req->send();
    //           $routes = json_decode($response->getBody())->response;
    //       } catch (HttpException $ex) {
    //           echo $ex;
    //           dd();
    //       }
    //
    //       foreach ($routes as $r) {
    //           if (str_contains($r->route_id, $version->version)) {
    //               $route = App\Route::find($r->route_id);
    //               if (is_null($route)) {
    //                   $route = new App\Route;
    //                   $route->create([
    //                       'id' => $r->route_id,
    //                       'route_id' => str_replace('-' . $version->version, '', $r->route_id),
    //                       'agency_id' => $r->agency_id,
    //                       'short_name' => $r->route_short_name,
    //                       'long_name' => $r->route_long_name,
    //                       'desc' => $r->route_desc,
    //                       'type_id' => $r->route_type,
    //                       'url' => $r->route_url,
    //                       'color' => $r->route_color,
    //                       'text-color' => $r->route_text_color,
    //                       'version' => $version->version
    //                   ]);
    //               }
    //           }
    //       }
    //
    //       // -- CALENDAR
    //       $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/calendar');
    //       $req->setHeader([
    //           'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       ])->setMethod(HTTP_Request2::METHOD_GET);
    //
    //       try {
    //           $response = $req->send();
    //           $calendars = json_decode($response->getBody())->response;
    //       } catch (HttpException $ex) {
    //           echo $ex;
    //           dd();
    //       }
    //
    //       foreach ($calendars as $c) {
    //           if (str_contains($c->service_id, $version->version)) {
    //               $calendar = App\Calendar::find($c->service_id);
    //               if (is_null($calendar)) {
    //                   $calendar = new App\Calendar;
    //                   $calendar->create([
    //                       'id' => $c->service_id,
    //                       'service_id' => str_replace('-' . $version->version, '', $c->service_id),
    //                       'monday' => (bool)$c->monday,
    //                       'tuesday' => (bool)$c->tuesday,
    //                       'wednesday' => (bool)$c->wednesday,
    //                       'thursday' => (bool)$c->thursday,
    //                       'friday' => (bool)$c->friday,
    //                       'saturday' => (bool)$c->saturday,
    //                       'sunday' => (bool)$c->sunday,
    //                       'start_date' => $c->start_date,
    //                       'end_date' => $c->end_date,
    //                       'version' => $version->version
    //                   ]);
    //               }
    //           }
    //       }
    //
    //       // -- CALENDAR DATES
    //       $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/calendarDate');
    //       $req->setHeader([
    //           'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       ])->setMethod(HTTP_Request2::METHOD_GET);
    //
    //       try {
    //           $response = $req->send();
    //           $calendar_dates = json_decode($response->getBody())->response;
    //       } catch (HttpException $ex) {
    //           echo $ex;
    //           dd();
    //       }
    //
    //       foreach ($calendar_dates as $cd) {
    //           if (str_contains($cd->service_id, $version->version)) {
    //               $calendar_date = App\CalendarDate::firstOrCreate([
    //                   'service_id' => $cd->service_id,
    //                   'date' => $cd->date,
    //                   'exception_type' => $cd->exception_type
    //               ]);
    //           }
    //       }
    //
    //       // -- TRIPS
    //       $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/trips');
    //       $req->setHeader([
    //           'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       ])->setMethod(HTTP_Request2::METHOD_GET);
    //
    //       try {
    //           $response = $req->send();
    //           $trips = json_decode($response->getBody())->response;
    //       } catch (HttpException $ex) {
    //           echo $ex;
    //           dd();
    //       }
    //
    //       foreach ($trips as $t) {
    //           if (str_contains($t->trip_id, $version->version)) {
    //               $trip = App\Trip::find($t->trip_id);
    //               if (is_null($trip)) {
    //                   $trip = new App\Trip;
    //                   $trip->create([
    //                       'id' => $t->trip_id,
    //                       'route_id' => $t->route_id,
    //                       'service_id' => $t->service_id,
    //                       'trip_id' => str_replace('-' . $version->version, '', $t->trip_id),
    //                       'headsign' => $t->trip_headsign,
    //                       'short_name' => $t->trip_short_name,
    //                       'direction_id' => $t->direction_id,
    //                       'block_id' => $t->block_id,
    //                       'shape_id' => $t->shape_id,
    //                       'wheelchair_accessible' =>
    //                           (property_exists($t, 'wheelchair_accessible')) ? $t->wheelchair_accessible : null,
    //                       'bikes_allowed' =>
    //                           (property_exists($t, 'bikes_allowed')) ? $t->bikes_allowed : null,
    //                       'version' => $version->version
    //                   ]);
    //               }
    //           }
    //       }
    //
    //       // -- SHAPES
    //       $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/shapes');
    //       $req->setHeader([
    //           'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       ])->setMethod(HTTP_Request2::METHOD_GET);
    //
    //       try {
    //           $response = $req->send();
    //           $shapes = json_decode($response->getBody())->response;
    //       } catch (HttpException $ex) {
    //           echo $ex;
    //           dd();
    //       }
    //
    //       foreach ($shapes as $s) {
    //           if (str_contains($s->shape_id, $version->version)) {
    //               $shape = App\Trip::find($s->shape_id);
    //               if (is_null($shape)) {
    //                   $shape = new App\Shape;
    //                   $shape->create([
    //                       'id' => $s->shape_id,
    //                       'shape_id' => str_replace('-' . $version->version, '', $s->shape_id),
    //                       'lat' => $s->shape_pt_lat,
    //                       'lon' => $s->shape_pt_lon,
    //                       'pt_sequence' => $s->shape_pt_sequence,
    //                       'dist_traveled' => $s->shape_dist_traveled,
    //                       'version' => $version->version
    //                   ]);
    //               }
    //           }
    //       }
    //
    //       // -- SHAPES
    //       // $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/shapes');
    //       // $req->setHeader([
    //       //     'Ocp-Apim-Subscription-Key' => env('AT_API_KEY')
    //       // ])->setMethod(HTTP_Request2::METHOD_GET);
    //       //
    //       // try {
    //       //     $response = $req->send();
    //       //     $shapes = json_decode($response->getBody())->response;
    //       // } catch (HttpException $ex) {
    //       //     echo $ex;
    //       //     dd();
    //       // }
    //
    //       $row = 1;
    //
    //       if (($handle = fopen()))
    //
    //       foreach ($shapes as $s) {
    //           if (str_contains($s->shape_id, $version->version)) {
    //               $shape = App\Trip::find($s->shape_id);
    //               if (is_null($shape)) {
    //                   $shape = new App\Shape;
    //                   $shape->create([
    //                       'id' => $s->shape_id,
    //                       'shape_id' => str_replace('-' . $version->version, '', $s->shape_id),
    //                       'lat' => $s->shape_pt_lat,
    //                       'lon' => $s->shape_pt_lon,
    //                       'pt_sequence' => $s->shape_pt_sequence,
    //                       'dist_traveled' => $s->shape_dist_traveled,
    //                       'version' => $version->version
    //                   ]);
    //               }
    //           }
    //       }
      //}
    // }

});
