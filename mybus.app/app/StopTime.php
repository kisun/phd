<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class StopTime extends Model
{
  // These do not have IDs
  protected $primaryKey = null;
  public $incrementing = false;
  public $timestamps = false;

  /**
   * Get the trip that owns the model.
   *
   * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
   */
  public function trip()
  {
      return $this->belongsTo('App\Trip');
  }


  /**
   * Get the stop that owns the model.
   *
   * @return \Illuminate\Database\Eloquent\Relations\BelongsTo
   */
  public function stop()
  {
      return $this->belongsTo('App\Stop');
  }
}
