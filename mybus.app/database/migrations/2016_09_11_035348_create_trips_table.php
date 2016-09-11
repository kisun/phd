<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateTripsTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('trips', function (Blueprint $table) {
            $table->string('route_id');
            $table->string('version');
            $table->string('service_id');
            $table->string('trip_id');
            $table->string('headsign')->nullable();
            $table->string('short_name')->nullable();
            $table->enum('direction_id', [0, 1])->nullable();
            $table->string('block_id')->nullable();
            $table->string('shape_id')->nullable();
            $table->enum('wheelchair_accessible', [0, 1, 2])->nullable();
            $table->enum('bikes_allowed', [0, 1, 2])->nullable();

            $table->primary(['trip_id', 'version']);
        });

        Schema::create('wheelchair_access_types', function (Blueprint $table) {
            $table->integer('id')->primary();
            $table->string('desc');
        });
        Schema::create('bikes_allowed_types', function (Blueprint $table) {
            $table->integer('id')->primary();
            $table->string('desc');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('trips');
        Schema::dropIfExists('wheelchair_access_types');
        Schema::dropIfExists('bikes_allowed_types');
    }
}
