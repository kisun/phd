<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Stop extends Model
{
    protected $primaryKey = 'stop_id';
    public $incrementing = false;

    /**
     * Get the stop_times for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function stop_times()
    {
        return $this->hasMany('App\StopTime');
    }
}
