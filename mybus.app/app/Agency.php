<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Agency extends Model
{
    public $incrementing = false;
    public $timestamps = false;
    protected $guarded = ['id'];

    /**
     * Get the routes for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function routes()
    {
        return $this->hasMany('App\Route');
    }
}
