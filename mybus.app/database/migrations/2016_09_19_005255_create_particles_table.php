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
            $table->bigInteger('timestamp')->unsigned();
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
