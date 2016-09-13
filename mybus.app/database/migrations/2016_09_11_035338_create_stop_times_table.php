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

            $table->foreign('trip_id')
                  ->references('id')->on('trips')
                  ->onDelete('cascade');
            $table->foreign('stop_id')
                  ->references('id')->on('stops');
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
