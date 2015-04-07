require.config({
    waitSeconds: 60,

    deps: ['main'],

    paths: {
        'jquery': 'vendor/jquery',
        'backbone': 'vendor/backbone',
        'lodash': 'vendor/lodash',
        'backbone.layoutmanager': 'vendor/backbone.layoutmanager',
        'async': 'vendor/async',
    },

    map: {
        '*': {'underscore': 'lodash'}
    }
});

define(['async!http://maps.google.com/maps/api/js?sensor=false&key=AIzaSyAANdBZ_S2xW1LmoO1os2-HDmA8AhIyAio'], function() {
    console.log('google maps is loaded');
});