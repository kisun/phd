<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateStopTimesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('stop_times', function (Blueprint $table) {
            $table->string('trip_id');
            $table->string('arrival_time');
            $table->string('departure_time');
            $table->string('stop_id');
            $table->integer('stop_sequence')->unsigned();
            $table->string('stop_headsign')->nullable();
            $table->enum('pickup_type', [0, 1, 2, 3])->nullable();
            $table->enum('drop_off_type', [0, 1, 2, 3])->nullable();
            $table->float('shape_dist_traveled')->nullable();
            $table->boolean('timepoint')->nullable();
            $table->string('version');

            $table->primary(['trip_id', 'stop_id']);
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
        Schema::dropIfExists('stop_times');
    }
}
