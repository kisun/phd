<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateParticlesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('particles', function (Blueprint $table) {
            $table->increments('id');
            $table->string('vehicle_id');
            $table->float('distance_into_trip');
            $table->float('velocity');
            $table->integer('segment');
            $table->bigInteger('arrival_time')->unsigned()->nullable();
            $table->bigInteger('departure_time')->unsigned()->nullable();
            $table->integer('parent')->unsigned()->nullable();
            $table->float('lat');
            $table->float('lon');
            $table->string('trip_id');
            $table->bigInteger('timestamp')->unsigned();
            $table->boolean('active');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('particles');
    }
}
