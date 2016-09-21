<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class VehiclePosition extends Model
{
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
     * Get the particles for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function particles()
    {
        return $this->hasMany('App\Particle', 'vehicle_id', 'vehicle_id');
    }

}
