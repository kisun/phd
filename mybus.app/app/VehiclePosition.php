<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class VehiclePosition extends Model
{
    public $timestamps = false;
    protected $primaryKey = 'oid';

    /**
     * Get the trip that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function trip()
    {
        return $this->belongsTo('App\Trip');
    }

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
     * Get the trip_update record associated with the vehicle, if it exists.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasOne
     */
    public function getTripUpdate()
    {
        if (count($this->trip->trip_update)) {
            return $this->trip->trip_update()
                    ->with(['stop_time_updates' => function($query) {
                        $query->orderBy('stop_sequence', 'desc');
                    }])->first();
        } else {
            return null;
        }
    }



    /**
     * Get the particles for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function particles()
    {
        return $this->hasMany('App\Particle', 'vehicle_id', 'vehicle_id');
    }

}
