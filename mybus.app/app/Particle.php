<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Particle extends Model
{
    /**
     * Get the vehicle that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function vehicle()
    {
        return $this->belongsTo('App\VehiclePosition', 'vehicle_id', 'vehicle_id');
    }
}
