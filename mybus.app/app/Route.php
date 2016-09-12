<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Route extends Model
{
    public $incrementing = false;
    public $timestamps = false;

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


    /**
     * Get the route type that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function type()
    {
        return $this->belongsTo('App\RouteType', 'id', 'type_id');
    }
}
