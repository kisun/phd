<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class TripUpdate extends Model
{
    public $timestamps = false;
    protected $primaryKey = 'oid';

    /**
     * Get the stop_time_updates for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function stop_time_updates()
    {
        return $this->hasMany('App\StopTimeUpdate');
    }


    /**
     * Get the trip that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function trip()
    {
        return $this->belongsTo('App\Trip');
    }
}
