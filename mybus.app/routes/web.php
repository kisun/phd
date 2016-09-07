<?php

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| This file is where you may define all of the routes that are handled
| by your application. Just tell Laravel the URIs it should respond
| to using a Closure or controller method. Build something great!
|
*/

Route::get('/', function () {
    return view('welcome');
});

Auth::routes();
Route::get('/logout', function() {
  Auth::logout();
  return redirect('/')->with('status', 'We hope to see you soon!');
});

Route::get('/home', 'HomeController@index');


Route::get('/routes', 'RouteController@index');
Route::get('/routes/{route}', 'RouteController@show');
