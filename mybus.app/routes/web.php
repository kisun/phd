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



Route::get('/test', function() {
    require_once 'HTTP/Request2.php';

    $request = new Http_Request2('https://api.at.govt.nz/v2/public/realtime/tripupdates');
    $url = $request->getUrl();

    $headers = array(
        // Request headers
        'Ocp-Apim-Subscription-Key' => env('AT_KEY'),
        'Accept' => 'application/x-protobuf'
    );

    $request->setHeader($headers);

    $parameters = array(
        // Request parameters
        // 'callback' => 'application/x-protobuf',
        //'tripid' => '{string}',
        //'vehicleid' => '{string}',
    );

    $url->setQueryVariables($parameters);

    $request->setMethod(HTTP_Request2::METHOD_GET);

    // Request body
    //$request->setBody("{body}");

    try
    {
        $response = $request->send();
        dd($response->getBody());
    }
    catch (HttpException $ex)
    {
        echo $ex;
    }
});
