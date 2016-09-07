<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Route extends Model
{
    protected $primaryKey = 'route_id';
    public $incrementing = false;

    /**
     * Get the trips for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function trips()
    {
        return $this->hasMany('App\Trip');
    }


    /**
     * Get the agency that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function agency()
    {
        return $this->belongsTo('App\Agency');
    }
}
