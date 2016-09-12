<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Version extends Model
{
    protected $table = "gtfs_versions";
    public $timestamps = false;
    protected $primaryKey = 'version';
    public $incrementing = false;

    /**
     * Get the routes for the current version.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function routes()
    {
        return $this->hasMany('App\Route');
    }
}
