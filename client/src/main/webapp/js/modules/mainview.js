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
                zoom:7,
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
            this.polylineComplete = _.bind(this.polylineComplete, this);
            this.polylineUpdated = _.bind(this.polylineUpdated, this);
        },

        events: {
            'click .toggle_drawing' : 'toggleDrawing',
            'click .calculate_path' : 'calculatePath'
        },

        toggleDrawing: function(e) {
            if(e) {
                e.preventDefault();
            }
            if(this.drawing) {
                delete this.drawing;
                this.$el.find('.toggle_drawing').html('Start drawing');
                this.drawingManager.setDrawingMode(null);
            } else {
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
                    this.drawing = true;
                    this.drawingManager.setDrawingMode(google.maps.drawing.OverlayType.POLYLINE);
                    if(this.polyline) {
                        this.drawingManager.polylineOptions.path = this.polyline.getPath();
                    }
                    this.resetCalc();
                    this.clearPolyline();
                    this.$el.find('.toggle_drawing').html('Stop drawing');
                    this.$el.find('.plotted').empty();
                    this.$el.find('.calculated').empty();
                }, this));
            }
        },

        clearPolyline: function() {
            if(this.polyline) {
                this.polyline.setMap(null);
                google.maps.event.clearInstanceListeners(this.polyline);
                this.polyline = null;
            }
        },

        afterRender: function() {
            this.map = new google.maps.Map(this.$el.find("#googleMap").get(0), this.mapProp);
            this.drawingManager = new google.maps.drawing.DrawingManager({
                drawingControl: false,
                polylineOptions: {
                    clickable: true,
                    editable: true,
                    draggable: true
                }
            });
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
                    bounds: bounds,
                    clickable: false
                });

                this.drawingManager.setMap(this.map);
                google.maps.event.addListener(this.drawingManager, 'polylinecomplete', this.polylineComplete);

            }, this));
        },

        polylineComplete: function(polyline) {
            this.clearPolyline();
            if(this.drawing) {
                this.toggleDrawing();
            }
            this.polyline = polyline;
            google.maps.event.addListener(this.polyline, "dragend", this.polylineUpdated);
            google.maps.event.addListener(this.polyline.getPath(), "insert_at", this.polylineUpdated);
            google.maps.event.addListener(this.polyline.getPath(), "remove_at",this.polylineUpdated);
            google.maps.event.addListener(this.polyline.getPath(), "set_at", this.polylineUpdated);
            if(this.checkPolyline()) {
                this.updatePlotted(this.polyline.getPath().getArray());
            } else {
                this.updatePlotted([]);
            }
        },

        checkPolyline: function() {
            if(this.polylineInsideRectangle()) return true;
            alert('Some points of polyline is outside rectangle');
        },

        polylineInsideRectangle: function(polyline) {
            polyline = polyline ? polyline : this.polyline;
            if(!this.rectangle || !polyline) return;
            return _.all(polyline.getPath().getArray(), function(ll) {
                return this.rectangle.getBounds().contains(ll);
            }, this);
        },

        polylineUpdated: function() {
            if(this.checkPolyline()) {
                this.updatePlotted(this.polyline.getPath().getArray());
            } else {
                this.updatePlotted([]);
            }
            this.resetCalc();
        },

        updatePlotted: function(array) {
            this.$el.find('.plotted').html(_.map(array, function(ll) {
                return '<li>' + ll.lat().toFixed(6) + ' ' + ll.lng().toFixed(6) + '</li>';
            }, this));
        },

        calculatePath: function(e) {
            if(e) {
                e.preventDefault();
            }
            if(this.calcing || !this.polyline) return;
            if(this.calcIndex == this.polyline.getPath().getArray().length - 1) return;
            this.calcing = true;
            this.calcIndex = _.isNumber(this.calcIndex) ? (this.calcIndex + 1) : 0;
            var pe = this.$el.find('.plotted li:eq(' + this.calcIndex + ')');
            pe.addClass('calcing');
            var ll = this.polyline.getPath().getArray()[this.calcIndex];
            $.ajax(app.root + 'path/' + this.pathId + '/calcPath', {
                method: 'POST',
                data: JSON.stringify({lat: ll.lat(), lng: ll.lng()})
            }).then(_.bind(function(s) {
                this.$el.find('.calculated').html(_.map(s, function(ll) {
                    return '<li>' + ll[0].toFixed(6) + ' ' + ll[1].toFixed(6) + '</li>';
                }));
                pe.removeClass('calcing');
                delete this.calcing;
            }, this));
        },

        resetCalc: function() {
            if(!_.isNumber(this.calcIndex)) return;
            $.ajax(app.root + 'path/' + this.pathId + '/resetCalc', {
                method: 'GET'
            }).then(_.bind(function(s) {
                this.$el.find('.calculated').empty();
                delete this.calcing;
                delete this.calcIndex;
            }, this));
        }
    });
    return Mainview;
})