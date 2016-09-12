<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateShapesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('shapes', function (Blueprint $table) {
            $table->string('id');
            $table->string('shape_id');
            $table->string('lat');
            $table->string('lon');
            $table->integer('pt_sequence')->unsigned();
            $table->float('dist_traveled')->nullable();
            $table->string('version');

            $table->primary(['shape_id', 'pt_sequence']);
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
        Schema::dropIfExists('shapes');
    }
}
