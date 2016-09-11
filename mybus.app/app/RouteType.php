<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class RouteType extends Model
{
    /**
     * Get the routes for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function routes()
    {
        return $this->hasMany('App\Route', 'type_id');
    }
}
