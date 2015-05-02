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
            this.checkElevation = _.bind(this.checkElevation, this);
        },

        events: {
            'click .toggle_drawing' : 'toggleDrawing',
            'click .calculate_path' : 'calculatePath',
            'click .toggle_elev' : 'toggleElev'
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
                    this.clearDrawing();
                    this.drawing = true;
                    this.drawingManager.setDrawingMode(google.maps.drawing.OverlayType.POLYLINE);
                    if(this.polyline) {
                        this.drawingManager.polylineOptions.path = this.polyline.getPath();
                    }
                    if(this.elev) {
                        this.toggleElev();
                    }
                }, this));
            }
        },

        clearDrawing: function() {
            this.resetCalc();
            this.clearPolyline();
            this.$el.find('.toggle_drawing').html('Stop drawing');
            this.$el.find('.plotted').empty();
            this.$el.find('.calculated').empty();
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
            google.maps.event.addListener(this.drawingManager, 'polylinecomplete', this.polylineComplete);
            this.loadClosestElevationIfChanged();
            this.centerChangedListener = google.maps.event.addListener(this.map, 'center_changed', this.centerChanged);
        },

        centerChanged: function() {
            console.log('center changed');
            this.loadClosestElevationIfChanged();
        },

        loadClosestElevationIfChanged: function() {
            if(this.pathId) return;
            var center = this.map.getCenter();
            if(this.polygon && google.maps.geometry.poly.containsLocation(center, this.polygon)) return;
            $.ajax(app.root + 'loadClosestElevationIfChanged', {
                method: 'POST',
                data: JSON.stringify({lat: center.lat(), lng: center.lng()})
            }).then(_.bind(function(s) {
                var paths = [
                    new google.maps.LatLng(s.nw[0], s.nw[1]),
                    new google.maps.LatLng(s.ne[0], s.ne[1]),
                    new google.maps.LatLng(s.se[0], s.se[1]),
                    new google.maps.LatLng(s.sw[0], s.sw[1])
                ];
                if(this.polygon) {
                    if(this.polygon.getPaths().equals(paths)) return;
                    this.polygon.setMap(null);
                    delete this.polygon;
                }
                this.polygon = new google.maps.Polygon({
                    strokeColor: '#FF0000',
                    strokeOpacity: 0.8,
                    strokeWeight: 2,
                    fillColor: '#FF0000',
                    fillOpacity: 0.35,
                    map: this.map,
                    paths: paths,
                    clickable: false
                });

                this.drawingManager.setMap(this.map);
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
            alert('Some points of polyline is outside p');
        },

        polylineInsideRectangle: function(polyline) {
            polyline = polyline ? polyline : this.polyline;
            if(!this.polygon || !polyline) return;
            return _.all(polyline.getPath().getArray(), function(ll) {
                return google.maps.geometry.poly.containsLocation(ll, this.polygon);
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
            if(_.isNumber(this.calcIndex)) {
                this.calcIndex = this.calcIndex + 1;
            } else {
                this.calcIndex = 0;
                this.donePolyline = new google.maps.Polyline({
                    strokeColor: '#FFFF00',
                    strokeOpacity: 1.0,
                    strokeWeight: 2,
                    zIndex: 2
                });
                this.donePolyline.setMap(this.map);
                this.calcedPolyline = new google.maps.Polyline({
                    strokeColor: '#00FFFF',
                    strokeOpacity: 1.0,
                    strokeWeight: 2,
                    zIndex: 1
                });
                this.calcedPolyline.setMap(this.map);
            }

            var pe = this.$el.find('.plotted li:eq(' + this.calcIndex + ')');
            pe.addClass('calcing');
            var ll = this.polyline.getPath().getArray()[this.calcIndex];
            $.ajax(app.root + 'path/' + this.pathId + '/calcPath', {
                method: 'POST',
                data: JSON.stringify({lat: ll.lat(), lng: ll.lng()})
            }).then(_.bind(function(timeCalcedPath) {
                this.donePolyline.getPath().push(ll);
                this.calcedPolyline.setPath(_.map(timeCalcedPath[1], function(ll) {
                    return new google.maps.LatLng(ll[0], ll[1]);
                }));
                this.$el.find('.calculated').html(_.map(timeCalcedPath[1], function(ll) {
                    return '<li>' + ll[0].toFixed(6) + ' ' + ll[1].toFixed(6) + '</li>';
                }));
                this.$el.find('.calc_time').append('<li>' + timeCalcedPath[0] + 'millis</li>');
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
                this.donePolyline.setMap(null);
                delete this.donePolyline;
                this.calcedPolyline.setMap(null);
                delete this.calcedPolyline;
            }, this));
        },

        toggleElev: function(e) {
            if(e) {
                e.preventDefault();
            }
            if(!this.pathId) return;
            if(this.elev) {
                delete this.elev;
                this.$el.find('.toggle_elev').html('Start elevation checking');
                google.maps.event.removeListener(this.checkElevationListener);

            } else {
                this.elev = true;
                this.$el.find('.toggle_elev').html('Stop elevation checking');
                if(this.drawing) {
                    this.toggleDrawing();
                }
                this.checkElevationListener = google.maps.event.addListener(this.map, 'click', this.checkElevation);
            }
        },

        checkElevation: function(e) {
            console.log('check elevation ' + e.latLng.lat() + ' ' + e.latLng.lng());
            $.ajax(app.root + 'path/' + this.pathId + '/getElevation', {
                method: 'POST',
                data: JSON.stringify({lat: e.latLng.lat(), lng: e.latLng.lng()})
            }).then(_.bind(function(elevation) {
                this.$el.find('.checked_elev').html(e.latLng.lat() + ' ' + e.latLng.lng() + ' ' + elevation);
            }, this));
        }
    });
    return Mainview;
})