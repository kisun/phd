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
            $table->string('id')->primary();
            $table->string('route_id');
            $table->string('service_id');
            $table->string('trip_id');
            $table->string('headsign')->nullable();
            $table->string('short_name')->nullable();
            $table->enum('direction_id', [0, 1])->nullable();
            $table->string('block_id')->nullable();
            $table->string('shape_id')->nullable();
            $table->enum('wheelchair_accessible', [0, 1, 2])->nullable();
            $table->enum('bikes_allowed', [0, 1, 2])->nullable();
            $table->string('version');

            $table->foreign('version')
                  ->references('version')->on('gtfs_versions')
                  ->onDelete('cascade');
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
    }
}
