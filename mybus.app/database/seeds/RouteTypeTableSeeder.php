<?php

use Illuminate\Database\Seeder;

class RouteTypeTableSeeder extends Seeder
{
    /**
     * Run the database seeds.
     *
     * @return void
     */
    public function run()
    {
        DB::table('route_types')->insert([
            'id' => 0,
            'name' => 'Tram, Streetcar, Light rail',
            'desc' => 'Any light rail or street level system within a metropolitan area.'
        ]);
        DB::table('route_types')->insert([
            'id' => 1,
            'name' => 'Subway, Metro',
            'desc' => 'Any underground rail system within a metropolitan area.'
        ]);
        DB::table('route_types')->insert([
            'id' => 2,
            'name' => 'Rail',
            'desc' => 'Used for intercity or long-distance travel.'
        ]);
        DB::table('route_types')->insert([
            'id' => 3,
            'name' => 'Bus',
            'desc' => 'Used for intercity or long-distance travel.'
        ]);
        DB::table('route_types')->insert([
            'id' => 4,
            'name' => 'Ferry',
            'desc' => 'Used for short- and long-distance boat service.'
        ]);
        DB::table('route_types')->insert([
            'id' => 5,
            'name' => 'Cable car',
            'desc' => 'Used for street-level cable cars where the cable runs beneath the car.'
        ]);
        DB::table('route_types')->insert([
            'id' => 6,
            'name' => 'Gondola, Suspended cable car',
            'desc' => 'Typically used for aerial cable cars where the car is suspended from the cable.'
        ]);
        DB::table('route_types')->insert([
            'id' => 7,
            'name' => 'Funicular',
            'desc' => 'Any rail system designed for steep inclines.'
        ]);
    }
}
