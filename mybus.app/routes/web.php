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
    return view('index');
});

Auth::routes();
Route::get('/logout', function() {
    Auth::logout();
    return redirect('/')->with('status', 'We hope to see you soon!');
});

Route::get('/home', 'HomeController@index');


Route::get('/routes', 'RouteController@index');
Route::get('/routes/{route}', 'RouteController@show');

Route::get('/trips/{trip}', 'TripController@show');
Route::get('/shapes/{id}', 'ShapeController@show');

Route::get('/status', function() {
    return view('status.index');
});

Route::get('/search', 'SearchController@search');

Route::get('/d3', function() {
    return view('d3');
});



Route::get('/symbols', function() {
    return view('symbols');
});
