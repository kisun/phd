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
    $versions = App\Version::orderBy('startdate', 'desc')->get();

    $req = new HTTP_Request2('https://api.at.govt.nz/v2/gtfs/versions');
    $url = $req->getUrl();
    $req->setHeader([
        'Ocp-Apim-Subscription-Key' => env('AT_API_KEY'),
    ]);

    $req->setMethod(HTTP_Request2::METHOD_GET);
    try {
        $response = $req->send();
        $new_versions = json_decode($response->getBody(), true)['response'];
    } catch (HttpException $ex) {
        echo $ex;
        dd();
    }

    // Check if each one is already in the data base :
    // DB::table('gtfs_versions')->delete();
    foreach ($new_versions as $new_version) {

        $version = App\Version::firstOrNew($new_version);
        if (is_null($version->id)) {
            // New ID - add it and then insert new data!
            $version->save();

            // Execute download of GTFS files - if the folder doesn't exist!
            if ( ! file_exists(storage_path() . '/gtfs.zip' )) {
                file_put_contents(storage_path() . '/gtfs.zip',
                                  fopen("https://cdn01.at.govt.nz/data/gtfs.zip", "r"));
            }


            // and then we need to do ALL the work!

            // -- AGENCIES
            $table = Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('agency.txt');
            $table = array_map("str_getcsv", explode("\r\n", $table));
            if (count(end($table)) == 1) {
                unset($table[count($table)-1]);
            }
            array_walk($table, function(&$a) use ($table) {
                $a = array_combine($table[0], $a);
            });
            array_shift($table);

            foreach ($table as $a) {
                $a = (object) $a;
                $agency = App\Agency::where('agency_id', $a->agency_id)->firstOrCreate([
                    'agency_id' => $a->agency_id,
                    'name' => $a->agency_name,
                    'url' => $a->agency_url,
                    'timezone' => $a->agency_timezone,
                    'lang' => $a->agency_lang,
                    'phone' => $a->agency_phone
                ]);
            }


            // -- ROUTES
            $table = Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('routes.txt');
            $table = array_map("str_getcsv", explode("\r\n", $table));
            if (count(end($table)) == 1) {
                unset($table[count($table)-1]);
            }
            array_walk($table, function(&$a) use ($table) {
                $a = array_combine($table[0], $a);
            });
            array_shift($table);

            foreach ($table as $r) {
                $r = (object) $r;
                if (str_contains($r->route_id, $version->version)) {
                    // Only go ahead if its the version we are interested in:
                    $route = App\Route::find($r->route_id);
                    if (is_null($route)) {
                        $route = new App\Route;
                        $route->create([
                            'id' => $r->route_id,
                            'route_id' => str_replace('-' . $version->version, '', $r->route_id),
                            'agency_id' => !empty($r->agency_id) ? $r->agency_id : null,
                            'short_name' => $r->route_short_name,
                            'long_name' => $r->route_long_name,
                            'desc' => !empty($r->route_desc) ? $r->route_desc : null,
                            'type_id' => $r->route_type,
                            'url' => !empty($r->route_url) ? $r->route_url : null,
                            'color' => !empty($r->route_color) ? $r->route_color : null,
                            'text_color' => !empty($r->route_text_color) ? $r->route_text_color : null,
                            'version_id' => $version->id
                        ]);
                    }
                }
            }


            // -- TRIPS
            $table = Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('trips.txt');
            $table = array_map("str_getcsv", explode("\r\n", $table));
            if (count(end($table)) == 1) {
                unset($table[count($table)-1]);
            }
            array_walk($table, function(&$a) use ($table) {
                $a = array_combine($table[0], $a);
            });
            array_shift($table);

            foreach ($table as $t) {
                $t = (object) $t;
                if (str_contains($t->trip_id, $version->version)) {
                    $trip = App\Trip::find($t->trip_id);
                    if (is_null($trip)) {
                        $trip = new App\Trip;
                        $trip->create([
                            'id' => $t->trip_id,
                            'route_id' => $t->route_id,
                            'service_id' => $t->service_id,
                            'trip_id' => str_replace('-' . $version->version, '', $t->trip_id),
                            'headsign' => !empty($t->trip_headsign) ? $t->trip_headsign : null,
                            'short_name' => !empty($t->trip_short_name) ? $t->trip_short_name : null,
                            'direction_id' => !empty($t->direction_id) ? $t->direction_id : null,
                            'block_id' => !empty($t->block_id) ? $t->block_id : null,
                            'shape_id' => !empty($t->shape_id) ? $t->shape_id : null,
                            'wheelchair_accessible' =>
                                !empty($t->wheelchair_accessible) ? $t->wheelchair_accessible : null,
                            'bikes_allowed' =>
                                !empty($t->bikes_allowed) ? $t->bikes_allowed : null,
                            'version_id' => $version->id
                        ]);
                    }
                }
            }

        } // temporary
        if (TRUE) {

            // -- SHAPES
            // more difficult, because the file is large -- do it in chunks
            $shapes = fopen(storage_path() . '/shapes.csv', "w");
            fwrite($shapes, Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('shapes.txt'));
            fclose($shapes);

            DB::statement('CREATE TABLE raw_shapes ( shape_id VARCHAR(255), ' .
                          'shape_pt_lat VARCHAR(255), shape_pt_lon VARCHAR(255), shape_pt_sequence INTEGER, ' .
                          'shape_dist_traveled DOUBLE PRECISION );');

            DB::statement("COPY raw_shapes FROM '" . storage_path() . "/shapes.csv' CSV HEADER;");
            //
            // DB::statement("INSERT INTO shapes (id, shape_id, pt)" .
            //               "  SELECT regexp_replace(shape_id, '-.+$', '') FROM raw_shapes");


            // DB::statement('DROP TABLE raw_shapes');

            // $shapes = fopen(storage_path() . '/shapes.csv', "r");
            // $header = fgetcsv($shapes);
            //
            // // foreach ($table as $s) {
            // while ( ! feof($shapes) ) {
            //     $s = fgetcsv($shapes);
            //     $s = (object) array_combine($header, $s);
            //     if (str_contains($s->shape_id, $version->version)) {
            //         $shape = App\Trip::find($s->shape_id);
            //         if (is_null($shape)) {
            //             $shape = new App\Shape;
            //             $shape->create([
            //                 'id' => $s->shape_id,
            //                 'shape_id' => str_replace('-' . $version->version, '', $s->shape_id),
            //                 'lat' => $s->shape_pt_lat,
            //                 'lon' => $s->shape_pt_lon,
            //                 'pt_sequence' => $s->shape_pt_sequence,
            //                 'dist_traveled' => !empty($s->shape_dist_traveled) ? $s->shape_dist_traveled : null,
            //                 'version_id' => $version->id
            //             ]);
            //         }
            //     }
            // }
            // fclose($shapes);
            // unlink(storage_path() . '/shapes.csv');
        } else { // temporary

            // -- CALENDAR
            $table = Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('calendar.txt');
            $table = array_map("str_getcsv", explode("\r\n", $table));
            if (count(end($table)) == 1) {
                unset($table[count($table)-1]);
            }
            array_walk($table, function(&$a) use ($table) {
                $a = array_combine($table[0], $a);
            });
            array_shift($table);

            foreach ($table as $c) {
                $c = (object) $c;
                if (str_contains($c->service_id, $version->version)) {
                    $calendar = App\Calendar::find($c->service_id);
                    if (is_null($calendar)) {
                        $calendar = new App\Calendar;
                        $calendar->create([
                            'id' => $c->service_id,
                            'service_id' => str_replace('-' . $version->version, '', $c->service_id),
                            'monday' => (bool)$c->monday,
                            'tuesday' => (bool)$c->tuesday,
                            'wednesday' => (bool)$c->wednesday,
                            'thursday' => (bool)$c->thursday,
                            'friday' => (bool)$c->friday,
                            'saturday' => (bool)$c->saturday,
                            'sunday' => (bool)$c->sunday,
                            'start_date' => $c->start_date,
                            'end_date' => $c->end_date,
                            'version_id' => $version->id
                        ]);
                    }
                }
            }


            // -- CALENDAR DATES
            $table = Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('calendar_dates.txt');
            $table = array_map("str_getcsv", explode("\r\n", $table));
            if (count(end($table)) == 1) {
                unset($table[count($table)-1]);
            }
            array_walk($table, function(&$a) use ($table) {
                $a = array_combine($table[0], $a);
            });
            array_shift($table);

            foreach ($table as $cd) {
                $cd = (object) $cd;
                if (str_contains($cd->service_id, $version->version)) {
                    $calendar_date = App\CalendarDate::firstOrCreate([
                        'service_id' => $cd->service_id,
                        'date' => $cd->date,
                        'exception_type' => $cd->exception_type
                    ]);
                }
            }
        }
    }

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
