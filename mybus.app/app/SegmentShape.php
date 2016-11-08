<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class SegmentShape extends Model
{
    public $primaryKey = "oid";
    public $timestamps = false;
    protected $guarded = [];

    /**
     * Get the segment_info that owns the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
     */
    public function segment_info()
    {
        return $this->belongsTo('App\SegmentInfo', 'segment_id', 'id');
    }

}
