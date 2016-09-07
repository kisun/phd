<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Agency extends Model
{
    protected $primaryKey = 'agency_id';
    public $incrementing = false;
    protected $table = 'agency';

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
