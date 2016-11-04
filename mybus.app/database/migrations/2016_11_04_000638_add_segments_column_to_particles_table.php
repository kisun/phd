<?php

use Illuminate\Support\Facades\Schema;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class AddSegmentsColumnToParticlesTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::table('particles', function (Blueprint $table) {
            $table->renameColumn('segment', 'stop_index');
            $table->integer('segment_index')->unsigned()->nullable();
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::table('particles', function (Blueprint $table) {
            $table->dropColumn('segment_index');
            $table->renameColumn('stop_index', 'segment');
        });
    }
}
