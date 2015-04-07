define([
    'app',
    'modules/mainview'
],

function(app, Mainview) {
    var Router = Backbone.Router.extend({
        initialize: function() {
            console.log('router init');
            app.view = new Mainview.View({el: '#main'});
            app.view.render();
        }
    });
    return Router;
});