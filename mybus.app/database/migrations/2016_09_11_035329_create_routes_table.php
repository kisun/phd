<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateRoutesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * I've split the ID into the ROUTE ID and the VERSION.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('route_types', function (Blueprint $table) {
            $table->integer('id')->primary();
            $table->string('name');
            $table->string('desc');
        });
        Schema::create('routes', function (Blueprint $table) {
            $table->string('id')->primary();
            $table->string('route_id');
            $table->string('agency_id')->nullable();
            $table->string('short_name');
            $table->string('long_name');
            $table->string('desc')->nullable();
            $table->integer('type_id');
            $table->string('url')->nullable();
            $table->string('color', 6)->nullable();
            $table->string('text_color', 6)->nullable();
            $table->integer('version_id');

            $table->foreign('agency_id')
                  ->references('agency_id')->on('agencies');
            $table->foreign('type_id')
                  ->references('id')->on('route_types');
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
        Schema::dropIfExists('routes');
        Schema::dropIfExists('route_types');
    }
}
