<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateStopsTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('stops', function (Blueprint $table) {
            $table->string('id')->primary();
            $table->string('stop_id');
            $table->string('code')->nullable();
            $table->string('name');
            $table->string('desc')->nullable();
            $table->string('lat');
            $table->string('lon');
            $table->string('zone_id')->nullable();
            $table->string('url')->nullable();
            $table->enum('location_type', [0, 1])->nullable();
            $table->string('parent_station')->nullable();
            $table->string('timezone')->nullable();
            $table->enum('wheelchair_boarding', [0, 1, 2])->nullable();
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
        Schema::dropIfExists('stops');
    }
}
