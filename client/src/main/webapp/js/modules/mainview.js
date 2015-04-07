define([
    'app'
],

function(app) {
    var Mainview = app.module();

    var defaultLocation = [59.930018, 10.710050];

    Mainview.View = Backbone.View.extend({
        template: 'mainview',

        initialize: function(options) {
            console.log('mainview init');
            this.mapProp = {
                center: new google.maps.LatLng(defaultLocation[0], defaultLocation[1]),
                zoom:10,
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
            $.ajax(app.root + 'loadClosestElevationIfChanged', {
                data: {
                    center: [center.lat(), center.lng()]
                }
            })
            console.log(center);
        }
    });
    return Mainview;
})