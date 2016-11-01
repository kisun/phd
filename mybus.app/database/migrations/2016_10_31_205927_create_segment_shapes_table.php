<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateSegmentShapesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('segment_shapes', function (Blueprint $table) {
            $table->string('id');
            $table->string('shape_id');
            $table->integer('leg')->unsigned();
            $table->integer('segment_id')->unsigned();
            $table->float('dist_traveled')->nullable();
            $table->integer('version_id');

            $table->primary(['id', 'leg']);
            $table->foreign('version_id')
                  ->references('id')->on('gtfs_versions')
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
        Schema::dropIfExists('segment_shapes');
    }
}
