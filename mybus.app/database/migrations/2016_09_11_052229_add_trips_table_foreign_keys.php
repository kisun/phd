<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class AddTripsTableForeignKeys extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::table('trips', function (Blueprint $table) {
            $table->foreign(['route_id', 'version'])
                  ->references(['route_id', 'version'])->on('routes');
            $table->foreign(['service_id', 'version'])
                  ->references(['service_id', 'version'])->on('calendars');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::table('trips', function (Blueprint $table) {
            $table->dropForeign(['route_id', 'version']);
            $table->dropForeign(['service_id', 'version']);
        });
    }
}
