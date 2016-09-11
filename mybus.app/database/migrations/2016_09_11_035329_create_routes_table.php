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
        Schema::create('routes', function (Blueprint $table) {
            $table->string('route_id');
            $table->string('version');
            $table->string('agency_id')->nullable();
            $table->string('short_name');
            $table->string('long_name');
            $table->string('desc')->nullable();
            $table->enum('type', [0, 1, 2, 3, 4, 5, 6, 7]);
            $table->string('url')->nullable();
            $table->string('color', 6)->nullable();
            $table->string('text-color', 6)->nullable();

            $table->primary(['route_id', 'version']);
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
    }
}
