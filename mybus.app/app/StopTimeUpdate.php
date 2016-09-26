<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class StopTimeUpdate extends Model
{
    public $timestamps = false;
    protected $primaryKey = 'oid';

    /**
     * Get the trip_update that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function trip_update()
    {
        return $this->belongsTo('App\TripUpdate', 'oid', 'trip_update_id');
    }
}
