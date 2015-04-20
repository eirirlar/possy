define([
    'backbone.layoutmanager'
], function() {
    var app = {};

    app.root = 'http://localhost:8080/possy/';
    app.dir = '';
    if('file:' == location.protocol) {
        var l = location.href.split('/');
        l.pop();
        app.dir = l.join('/') + '/';
    }

    app.module = function() {
        return {Views: {}, Models: {}};
    };

    $.ajaxSetup({
        crossDomain: true
        //,xhrFields: {withCredentials: true}
    });

    return app;
})