
/**
 * First we will load all of this project's JavaScript dependencies which
 * include Vue and Vue Resource. This gives a great starting point for
 * building robust, powerful web applications using Vue and Laravel.
 */

require('./bootstrap');
window.moment = require('moment');
require("moment-duration-format");


/**
 * Next, we will create a fresh Vue application instance and attach it to
 * the body of the page. From here, you may begin adding components to
 * the application, or feel free to tweak this setup for your needs.
 */

// Vue.component('example', require('./components/Example.vue'));
//
// const app = new Vue({
//     el: 'body'
// });
// Vue.component('alert', require('./components/FlashMessage.vue'));

// const alert = new Vue({
//     el: 'body'
// });



$(document).ready(function() {
  $(".nav-toggle").on('click', function(e) {
    e.preventDefault();
    $(this).toggleClass('toggled');
    if ($(this).hasClass('toggled')) {
      $(".app-nav").addClass('slideout');
    } else {
      $(".app-nav").removeClass('slideout');
    }
  });

  $(".app-content").on('click', function() {
    $(".nav-toggle").removeClass('toggled');
    $(".app-nav").removeClass('slideout');
  });
});
