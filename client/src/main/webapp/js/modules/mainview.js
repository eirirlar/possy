define([
    'app',
    'modules/gmap'
],

function(app, gmap) {
    var Mainview = app.module();

    var defaultLocation = [59.930018, 10.710050];

    Mainview.View = Backbone.View.extend({
        template: 'mainview',

        initialize: function(options) {
            console.log('mainview init');
            this.mapProp = {
                center: new google.maps.LatLng(defaultLocation[0], defaultLocation[1]),
                zoom:5,
                mapTypeId: google.maps.MapTypeId.ROADMAP
            };

            if(navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(_.bind(function (position) {
                    this.mapProp.center = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
                    if(this.map) {
                        this.map.setCenter(this.mapProp.center);
                    }
                }, this));
            }

            this.centerChanged = _.debounce(_.bind(this.centerChanged, this), 1000);
            this.rectangleClicked = _.bind(this.rectangleClicked, this);
        },

        afterRender: function() {
            this.map = new google.maps.Map(this.$el.find("#googleMap").get(0), this.mapProp);
            if(!this.pathId) {
                this.loadClosestElevationIfChanged();
                this.centerChangedListener = google.maps.event.addListener(this.map, 'center_changed', this.centerChanged);
            }
        },

        centerChanged: function() {
            console.log('center changed');
            this.loadClosestElevationIfChanged();
        },

        loadClosestElevationIfChanged: function() {
            var center = this.map.getCenter();
            if(this.rectangle && this.rectangle.getBounds().contains(center)) return;
            $.ajax(app.root + 'loadClosestElevationIfChanged', {
                method: 'POST',
                data: JSON.stringify({lat: center.lat(), lng: center.lng()})
            }).then(_.bind(function(s) {
                var bounds = new google.maps.LatLngBounds(
                    new google.maps.LatLng(s.lat0, s.lng0),
                    new google.maps.LatLng(s.lat1, s.lng1));
                if(this.rectangle) {
                    if(this.rectangle.getBounds().equals(bounds)) return;
                    this.rectangle.setMap(null);
                    delete this.rectangle;
                }
                this.rectangle = new google.maps.Rectangle({
                    strokeColor: '#FF0000',
                    strokeOpacity: 0.8,
                    strokeWeight: 2,
                    fillColor: '#FF0000',
                    fillOpacity: 0.35,
                    map: this.map,
                    bounds: bounds
                });

                google.maps.event.addListener(this.rectangle, 'rightclick', this.rectangleClicked);

            }, this));
            console.log(center);
        },

        rectangleClicked: function(event) {
            console.log('rectangle clicked ' + event.latLng);
            _.bind(function() {
                if(!this.pathId) {
                    console.log('path drawing started, getting path id');
                    return $.ajax(app.root + 'path', {
                        method: 'GET'
                    }).then(_.bind(function(pathId) {
                        console.log('got path id ' + pathId);
                        this.pathId = pathId;
                        app.router.navigate(this.pathId, {trigger:false});
                        this.$el.find('.info').prepend('<li>You are now editing unsaved path \'' + pathId + '\'</li>');

                        if(this.centerChangedListener) {
                            google.maps.event.removeListener(this.centerChangedListener);
                            delete this.centerChangedListener;
                        }
                        return pathId;
                    }, this));
                }
                return $.when(this.pathId);
            }, this)().then(_.bind(function(pathId) {
                $.ajax(app.root + 'path/' + pathId + '/addPoint', {
                    method: 'POST',
                    data: JSON.stringify({lat: event.latLng.lat(), lng: event.latLng.lng()})
                }).then(_.bind(function(s) {
                    this.$el.find('.calculated').html(_.map(s, function(ll) {
                        return '<li>' + ll[0] + '\n' + ll[1] + '</li>';
                    }));
                    this.$el.find('.plotted').prepend('<li>' + event.latLng.lat() + '\n' + event.latLng.lng() + '</li>');
                }, this));
            }, this));
        }
    });
    return Mainview;
})