<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class Segment extends Model
{
    public $timestamps = false;
    protected $guarded = [];

    /**
     * Get the segment_info that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function segment_info()
    {
        return $this->belongsTo('App\SegmentInfo', 'id', 'segment_id');
    }
}
