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
        },

        afterRender: function() {
            this.map = new google.maps.Map(this.$el.find("#googleMap").get(0), this.mapProp);
            this.loadClosestElevationIfChanged();
            google.maps.event.addListener(this.map, 'center_changed', this.centerChanged);
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
            if(!this.pathId) {
                console.log('path drawing started, getting path id');
                $.ajax(app.root + 'pathId', {
                    method: 'GET'
                }).then(_.bind(function(pathId) {
                    console.log('got path id ' + pathId);
                    this.pathId = pathId;
                }, this), function(f) {
                    console.log('error');
                });
            }
        }
    });
    return Mainview;
})