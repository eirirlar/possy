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