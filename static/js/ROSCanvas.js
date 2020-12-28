
/**
 * WebGL canvas for marker visualization.
 **/
function ROSCanvas(options){
    let that = this;
    
    // Prefix for mesh GET URL's
    const meshPath  = options.meshPath || '/';
    // sprite markers and render events
    let sprites = [];
    let render_event;

    // ROS messages
    this.tfClient = undefined;
    this.cameraPoseClient = undefined;
    this.snapshotTopic = undefined;
    // The selected marker object or undefined
    this.selectedMarker = undefined;
    this.markers = {};

    this.on_start_loading = options.on_start_loading || function(){};
    this.on_finished_loading = options.on_finished_loading || function(){};

    // add button bar
    this.canvas_links = $("<p>");
    this.canvas_links.addClass("icon-links marker-links");
    options.parent.append(this.canvas_links);
    // add links to the button bar
    var addLink = function(linkOpts) {
        const link = $("<a>");
        const icon = $("<i>");
        link.addClass("marker-link p-1");
        icon.addClass("fa " + linkOpts.icon);
        icon.prop('title', linkOpts.title);
        link.append(icon);
        that.canvas_links.append(link);
        return link;
    }
    this.link_screenshot = addLink({
        title: 'Screenshot',
        icon: 'fa-camera'});
    this.link_maximize = addLink({
        title: 'Maximize',
        icon: 'fa-window-maximize'});

    var aspect_ratio = 16.0/9.0;
    var canvas_width = 400;
    var canvas_height = canvas_width/aspect_ratio;
    var is_maximized = false;
    
    // The canvas object
    this.rosViewer = new EASEViewer({
        client: that,
        div : options.parent,
        width : canvas_width,
        height : canvas_height,
        antialias : true,
        background : options.background || '#ffffff',
        useShader : false,
        camera: {
            position: {x : 3, y : 3, z : 3},
            zoomSpeed: 0.5,
            near: 0.05,
            far: 100.0
        },
        sun: {
            pos: [-1, 0.5, 3.0],
            shadow: {
                debug: false
            }
        },
        spots: [
            {   pos: [0, 2.5, 4],
                target: [0, 2.5, 0],
                distance: 6.0 },
            {   pos: [0, -2.5, 4],
                target: [0, -2.5, 0],
                distance: 6.0 },
            {   pos: [2.5, 0, 4],
                target: [2.5, 0, 0],
                distance: 6.0 },
            {   pos: [-2.5, 0, 4],
                target: [-2.5, 0, 0],
                distance: 6.0 }
        ],
        on_start_loading: this.on_start_loading,
        on_finished_loading: this.on_finished_loading
    });
    // add some default objects to the scene
    this.rosViewer.scene.add(new ROS3D.Grid({
        color: '#cccccc',
        num_cells: 20,
        lineWidth: 1,
        cellSize: 1
    }));
    this.rosViewer.cameraControls.showAxes();

    this.resize = function () {
        // compute canvas size
        const max_width = options.parent.parent().parent().width()-4;
        const small_width = 400;
        if(!is_maximized && small_width<max_width) {
            canvas_width = 400;
        }
        else {
            canvas_width = max_width;
        }
        canvas_height = canvas_width/aspect_ratio;
        // resize
        that.rosViewer.resize(canvas_width, canvas_height);
        // HACK
        $('.progress').width(canvas_width * 0.8);
    };
    $(window).on('resize', that.resize);

    this.maximize = function () {
        is_maximized = !is_maximized;
        that.resize();
    };
    
    this.registerNodes = function(ros) {
        // topic used for publishing canvas snapshots
        that.snapshotTopic = new ROSLIB.Topic({
            ros : ros,
            name : '/openease/video/frame',
            messageType : 'sensor_msgs/Image'
        });
        // Setup a client to listen to TFs.
        that.tfClient = new ROSLIB.TFClient({
            ros : ros,
            angularThres : 0.01,
            transThres : 0.0001,
            rate : 10.0,
            // FIXME: get root frame from mongo (the one without parent)
            fixedFrame : 'map'
        });
        // Setup the /camera/pose client.
        that.cameraPoseClient = new ROSLIB.Topic({
            ros : ros,
            name : '/camera/pose',
            messageType : 'geometry_msgs/Pose'
        });
        that.cameraPoseClient.subscribe(function(message) {
            // TODO
            //if(that.on_camera_pose_received)
            //  that.on_camera_pose_received(message);
        });
    };

    this.addMarkerArray = function(arrayMessage){
        arrayMessage.markers.forEach(function(message) {
            var markerName = message.ns + message.id;
            if(message.action === 0) {
                var updated = false;
                if(markerName in that.markers) { // "MODIFY"
                    var markerItem = that.markers[markerName];
                    var marker = markerItem[0];
                    var node   = markerItem[1];
                    updated = marker.update(message);
                    if(!updated) { // "REMOVE"
                        node.unsubscribeTf();
                        that.rosViewer.removeMarker(marker,node);
                        delete that.markers[markerName];
                    }
                }
                if(!updated) { // "ADD"
                    var newMarker = new ROS3D.Marker({
                        message : message,
                        path : meshPath,
                    });
                    newMarker.id = message.id;
                    newMarker.ns = message.ns;
                    newMarker.frame_id = message.header.frame_id;
                    newMarker.marker_type = message.type;
                    // XXX: below flags are HACKS
                    newMarker.isSelectable = true;
                    newMarker.isSceneOrtho = false;
                    var newNode = new ROS3D.SceneNode({
                        frameID : message.header.frame_id,
                        tfClient : that.tfClient,
                        object : newMarker
                    });
                    that.markers[markerName] = [newMarker,newNode];
                    that.rosViewer.addMarker(newMarker,newNode);
                }
            }
            else if(message.action === 1) { // "DEPRECATED"
                console.warn('Received marker message with deprecated action identifier "1"');
            }
            else if(message.action === 2 && markerName in that.markers) { // "DELETE"
                var markerItem = that.markers[markerName];
                var marker = markerItem[0];
                var node   = markerItem[1];
                node.unsubscribeTf();
                that.rosViewer.removeMarker(marker,node);
                delete that.markers[markerName];
            }
            else if(message.action === 3) { // "DELETE ALL"
                for (var markerItem in that.markers){
                    var marker = markerItem[0];
                    var node   = markerItem[1];
                    node.unsubscribeTf();
                    that.rosViewer.removeMarker(marker,node);
                }
                that.markers = {};
            }
            else {
                console.warn('Received marker message with unknown action identifier "'+message.action+'"');
            }
        });
        //this.emit('change');
    };

    this.snapshot = function () {
        var a = document.createElement('a');
        // Without 'preserveDrawingBuffer' set to true, we must render now
        this.rosViewer.render();
        a.href = this.rosViewer.renderer.domElement.toDataURL().replace(
            "image/png", "image/octet-stream");
        a.download = 'canvas.png';
        a.click();
    };
    
    ///////////////////////////////
    //////////// ...
    ///////////////////////////////
    
    this.selectMarker = function(marker) {
        if(that.selectedMarker) {
            that.unselectMarker();
        }
        that.selectedMarker = marker;
        that.rosViewer.highlight(marker);
        // update follow-up widget with queries about selected marker
        selectTFFrame(marker.frame_id);
    };
    
    this.unselectMarker = function() {
        if(!that.selectedMarker) {
            return;
        }
        that.rosViewer.unhighlight(that.selectedMarker);
        that.selectedMarker = undefined;
    };
    
    this.removeMarker = function(marker) {
        if(marker === that.selectedMarker) {
            that.unselectMarker();
        }
    };
    
    ///////////////////////////////
    //////////// Camera
    ///////////////////////////////
    
    this.setCameraPose = function(pose) {
        that.setCameraPosition(pose.position);
        that.setCameraOrientation(pose.orientation);
    };
    
    this.setCameraPosition = function(position) {
        that.rosViewer.cameraControls.camera.position.x = position.x;
        that.rosViewer.cameraControls.camera.position.y = position.y;
        that.rosViewer.cameraControls.camera.position.z = position.z;
    };
    
    this.setCameraOrientation = function(o) {
        var orientation = new THREE.Quaternion(o.x, o.y,
                                               o.z, o.w);
        var frontVector = new THREE.Vector3(0, 0, 1);
        frontVector.applyQuaternion(orientation);
        that.rosViewer.cameraControls.center = that.rosViewer.cameraControls.camera.position.clone();
        that.rosViewer.cameraControls.center.add(frontVector);
    };
    
    ///////////////////////////////
    //////////// Sprites
    ///////////////////////////////
    
//     this.on_render = function(camera,scene) {
//         var index;
//         for(index = 0; index < sprites.length; index++) {
//             //sprites[index].camera = camera;
//             //render_event.target = sprites[index];
//             render_event.camera = camera;
//             sprites[index].dispatchEvent(render_event);
//         }
//     };
};
