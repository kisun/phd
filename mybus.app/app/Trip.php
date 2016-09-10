<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Trip extends Model
{
    protected $primaryKey = 'trip_id';
    public $incrementing = false;

    /**
     * Get the route that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function route()
    {
        return $this->belongsTo('App\Route');
    }


    /**
     * Get the stop_times for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function stop_times()
    {
        return $this->hasMany('App\StopTime');
    }

    public function start_time() {
        $firstStop = $this->stop_times()->where('stop_sequence', 1)->first();
        $time = \Carbon\Carbon::createFromFormat('H:i:s', $firstStop->departure_time);
        return $time->format('h:i a');
    }



    /**
     * Get the calendar that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function calendar()
    {
        return $this->belongsTo('App\Calendar', 'service_id', 'service_id');
    }


    /**
     * Get the shape points for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function shapes()
    {
        return $this->hasMany('App\Shape', 'shape_id', 'shape_id')
                ->orderBy('shape_pt_sequence');
    }

    /**
     * Get the shape for the model.
     *
     * @return JSON shapefile
     */
    public function shape()
    {
        return $this->shapes->toJson();
    }
}
