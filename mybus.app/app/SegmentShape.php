<?php

namespace App;

use Illuminate\Database\Eloquent\Model;

class SegmentShape extends Model
{
    public $primaryKey = "oid";
    public $timestamps = false;
    protected $guarded = [];
}
