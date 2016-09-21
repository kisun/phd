const elixir = require('laravel-elixir');

require('laravel-elixir-vue');

/*
 |--------------------------------------------------------------------------
 | Elixir Asset Management
 |--------------------------------------------------------------------------
 |
 | Elixir provides a clean, fluent API for defining some basic Gulp tasks
 | for your Laravel application. By default, we are compiling the Sass
 | file for our application, as well as publishing vendor resources.
 |
 */

elixir(mix => {
    mix.copy('node_modules/bootstrap-sass/assets/fonts/**', 'public/fonts')
       .copy('resources/assets/img/**', 'public/img')
       .sass('app.scss')
       .webpack('app.js');
    mix.browserSync({
        notify: false,
        proxy: 'mybus.app',
        // host: '192.168.0.13'
    });
});
