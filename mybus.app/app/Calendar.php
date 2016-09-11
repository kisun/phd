<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Calendar extends Model
{
    public $incrementing = false;

    /**
     * Get the trips for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function trips()
    {
        return $this->hasMany('App\Trip', 'service_id');
    }


    /**
     * Get the calendar_dates for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function calendar_dates()
    {
        return $this->hasMany('App\CalendarDate', 'service_id');
    }
}
