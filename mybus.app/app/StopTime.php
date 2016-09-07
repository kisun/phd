<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class StopTime extends Model
{
  // These do not have IDs
  /**
   * Get the trip that owns the model.
   *
   * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
   */
  public function trip()
  {
      return $this->belongsTo('App\Trip', 'trip_id');
  }


  /**
   * Get the stop that owns the model.
   *
   * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
   */
  public function stop()
  {
      return $this->belongsTo('App\Stop', 'stop_id', 'stop_id');
  }
}
