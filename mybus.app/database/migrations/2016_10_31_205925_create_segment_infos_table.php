<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateSegmentInfosTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('segment_infos', function (Blueprint $table) {
            $table->increments('id');
            $table->float('length');
            $table->integer('start_id')->unsigned()->nullable();
            $table->integer('end_id')->unsigned()->nullable();

            $table->foreign('start_id')
                  ->references('id')->on('intersections');
            $table->foreign('end_id')
                  ->references('id')->on('intersections');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('segment_infos');
    }
}
