<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class SegmentInfo extends Model
{
    public $timestamps = false;
    protected $guarded = [];

    /**
     * Get the segment_shapes for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function segment_shapes()
    {
        return $this->hasMany('App\SegmentShape', 'segment_id', 'id');
    }

    /**
     * Get the shape_points for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function shape_points()
    {
        return $this->hasMany('App\Segment', 'segment_id', 'id');
    }


    /**
     * Get the speeds for the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasMany
     */
    public function speed_history()
    {
        return $this->hasMany('App\SegmentSpeed', 'segment_id', 'id');
    }

    /**
     * Get the latest speed record associated with the model.
     *
     * @return \Illuminate\Database\Eloquent\Relations\HasOne
     */
    public function current_speed()
    {
        return $this->hasMany('App\SegmentSpeed', 'segment_id', 'id')
                ->where('current', true)
                ->orderBy('timestamp', 'desc');
    }
}
