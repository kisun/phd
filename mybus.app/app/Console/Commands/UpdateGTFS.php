<?php

namespace App\Console\Commands;

use Illuminate\Console\Command;

use App;
use DB;
use HTTP_Request2;
use Zipper;

class UpdateGTFS extends Command
{
    /**
     * The name and signature of the console command.
     *
     * @var string
     */
    protected $signature = 'gtfs:update';

    /**
     * The console command description.
     *
     * @var string
     */
    protected $description = 'Update the GTFS static database';

    /**
     * Create a new command instance.
     *
     * @return void
     */
    public function __construct()
    {
        parent::__construct();
    }

    /**
     * Execute the console command.
     *
     * @return mixed
     */
    public function handle()
    {
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


        foreach ($new_versions as $new_version) {

            $version = App\Version::firstOrNew($new_version);
            if (is_null($version->id)) {
                echo "*** Adding version $version->version\n\n";

                // New ID - add it and then insert new data!
                $version->save();

                // Execute download of GTFS files - if the folder doesn't exist!
                // TODO: use headers to check if gtfs.zip has been updated on AT's CDN
                if ( ! file_exists(storage_path() . '/gtfs.zip' )) {
                    echo ">>>>> Downloding GTFS data ... ";
                    file_put_contents(storage_path() . '/gtfs.zip',
                                      fopen("https://cdn01.at.govt.nz/data/gtfs.zip", "r"));
                    echo "done.\n";
                }


                // and then we need to do ALL the work!

                // -- AGENCIES
                echo "* Adding agencies ... ";
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
                echo "done.\n";


                // -- ROUTES
                echo "* Adding routes ... ";
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
                echo "done.\n";


                // -- TRIPS
                echo "* Adding trips ... ";
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
                echo "done.\n";


                // -- SHAPES
                echo "* Adding shapes ... ";
                // more difficult, because the file is large -- do it in chunks
                $shapes = fopen(storage_path() . '/shapes.csv', "w");
                fwrite($shapes, Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('shapes.txt'));
                fclose($shapes);

                DB::statement('CREATE TABLE raw_shapes ( shape_id VARCHAR(255), ' .
                              'shape_pt_lat VARCHAR(255), shape_pt_lon VARCHAR(255), shape_pt_sequence INTEGER, ' .
                              'shape_dist_traveled DOUBLE PRECISION );');

                echo "-> table ";
                DB::statement("COPY raw_shapes FROM '" . storage_path() . "/shapes.csv' CSV HEADER;");
                unlink(storage_path() . '/shapes.csv');

                echo "-> shapes ... ";
                DB::statement(
                    "INSERT INTO shapes (id, shape_id, lat, lon, pt_sequence, dist_traveled, version_id) " .
                    "SELECT shape_id AS id, regexp_replace(shape_id, '-.+$', '') AS shape_id, " .
                    "shape_pt_lat AS lat, shape_pt_lon AS lon, shape_pt_sequence AS pt_sequence, " .
                    "shape_dist_traveled AS dist_traveled, $version->id AS version_id " .
                    "FROM raw_shapes WHERE shape_id LIKE '%$version->id'");

                DB::statement('DROP TABLE raw_shapes');
                echo "done.\n";


                // -- SHAPES
                echo "* Adding stops ... ";
                $table = Zipper::make(storage_path() . '/gtfs.zip')->getFileContent('stops.txt');
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
                    $stop = App\Stop::firstOrCreate([
                        'id' => $t->stop_id,
                        'code' => !empty($t->stop_code) ? $t->stop_code : null,
                        'name' => $t->stop_name,
                        'desc' => !empty($t->stop_desc) ? $t->stop_desc : null,
                        'lat' => $t->stop_lat,
                        'lon' => $t->stop_lon,
                        'zone_id' => !empty($t->zone_id) ? $t->zone_id : null,
                        'url' => !empty($t->url) ? $t->url : null,
                        'location_type' => !empty($t->location_type) ? $t->location_type : null,
                        'parent_station' => !empty($t->parent_station) ? $t->parent_station : null,
                        'timezone' => !empty($t->timezone) ? $t->timezone : null,
                        'wheelchair_boarding' =>
                            !empty($t->wheelchair_boarding) ? $t->wheelchair_boarding : null,
                    ]);
                }
                echo "done.\n";


                echo "Adding stop times ... csv ";
                // more difficult, because the file is large -- do it in chunks
                $stop_times = fopen(storage_path() . '/stop_times.csv', "w");
                fwrite($stop_times, Zipper::make(storage_path() . '/gtfs.zip')
                                      ->getFileContent('stop_times.txt'));
                fclose($stop_times);

                DB::statement('CREATE TABLE tmp ( trip_id VARCHAR(255), ' .
                              'arrival_time VARCHAR(255), departure_time VARCHAR(255), '.
                              'stop_id VARCHAR(255), stop_sequence INTEGER, stop_headsign VARCHAR(255), ' .
                              'pickup_type VARCHAR(255), drop_off_type VARCHAR(255), '.
                              'shape_dist_traveled DOUBLE PRECISION)');

                echo "-> table ";
                DB::statement("COPY tmp FROM '" . storage_path() . "/stop_times.csv' CSV HEADER;");
                unlink(storage_path() . '/stop_times.csv');

                echo "-> stop_times ... ";
                DB::statement(
                    "INSERT INTO stop_times (trip_id,arrival_time,departure_time,stop_id,stop_sequence,".
                                        "stop_headsign,pickup_type,drop_off_type,shape_dist_traveled) " .
                    "SELECT * FROM tmp WHERE trip_id LIKE '%-$version->version'");

                DB::statement('DROP TABLE tmp');
                echo "done.\n";


                // -- CALENDAR
                echo "* Adding calendars ... ";
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
                echo "done.\n";


                // -- CALENDAR DATES
                echo "* Adding calendar dates ... ";
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
                echo "done.\n";
            }
        }
    }
}
