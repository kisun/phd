<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Trip extends Model
{
    public $incrementing = false;
    public $timestamps = false;
    protected $guarded = [];

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

    /**
     *  Get the start time of a trip
     *
     * @return \Carbon\Carbon time value.
     */
    public function start_time() {
        $firstStop = $this->stop_times()
                      ->where('stop_sequence', 1)->first();
        $time = \Carbon\Carbon::createFromFormat('H:i:s', $firstStop->departure_time);

        return $time;
    }



    /**
     * Get the calendar that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function calendar()
    {
        return $this->hasOne('App\Calendar', 'id', 'service_id');
    }


    /**
     * Get the shape points for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function getShape()
    {
        $shape = \App\SegmentShape::where('id', $this->shape_id)->orderBy('leg');
        if (count($shape)) {
            return $shape->with('segment_info.shape_points')->get();
        } else {
            return $shape = \App\Shape::where('id', $this->shape_id)->orderBy('pt_sequence')->get();
        }
        // $shape = \App\Shape::where('id', $this->shape_id)->orderBy('pt_sequence');
        // return $shape->get();
    }

    /**
     * The stops that belong to the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsToMany
     */
    public function stops()
    {
        return $this->belongsToMany('App\Stop', 'stop_times')
                    ->withPivot('arrival_time', 'departure_time', 'stop_sequence', 'shape_dist_traveled')
                    ->orderBy('stop_sequence');
    }


    /**
     * Get the vehicle record associated with the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasOne
     */
    public function vehicle_position()
    {
        return $this->hasOne('App\VehiclePosition');
    }


    /**
     * Get the trip_update record associated with the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasOne
     */
    public function trip_update()
    {
        return $this->hasOne('App\TripUpdate');
    }

}
