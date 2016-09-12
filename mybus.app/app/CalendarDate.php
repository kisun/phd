<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class CalendarDate extends Model
{
    public $timestamps = false;
    public $incrementing = false;

    /**
     * Get the calendar that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function calendar()
    {
        return $this->belongsTo('App\Calendar', 'service_id');
    }
}
