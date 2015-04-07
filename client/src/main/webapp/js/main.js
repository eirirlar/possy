require([
    'app',
    'router'
], function(app, Router) {
    console.log('main.js');
    $(document).on('click', 'a[href]:not([data-bypass])', function(e) {
        var href = { prop: $(this).prop('href'), attr: $(this).attr('href') };
        var root = location.protocol + '//' + location.host + app.root;
        if(href.prop.slice(0, root.length) === root) {
            e.preventDefault();
            Backbone.history.navigate(href.attr, true);
        }
    });

    var JST = window.JST = window.JST || {};
    var fetchings = {};

    Backbone.Layout.configure({
        manage: true,

        prefix: 'templates/',

        fetchTemplate: function(path) {
            path = path + '.html';
            if(JST[path]) {
                return JST[path];
            }
            var done = this.async();
            fetchings[path] || (fetchings[path] = []);
            if(fetchings[path].length == 0) {
                $.get(app.dir + path, function(contents) {
                    var jst = _.template(contents);
                    JST[path] = jst;
                    _.each(fetchings[path], function(fetching) {
                        fetching(jst);
                    });
                    delete fetchings[path];
                });
            }
            fetchings[path].push(done);
        }
    });

    app.router = new Router();
    Backbone.history.start({pushState: false, root: app.root});

});