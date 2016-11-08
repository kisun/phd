<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateSegmentSpeedsTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('segment_speeds', function (Blueprint $table) {
            $table->increments('id');
            $table->integer('segment_id')->unsigned();
            $table->float('speed_mean');
            $table->float('speed_var');
            $table->bigInteger('timestamp')->unsigned();
            $table->boolean('current');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('segment_speeds');
    }
}
