
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
            intensity: 0.66,
            color: '#eeeeee',
            pos: [-1, 0.5, 3.0],
            shadow: {
                debug: false
            }
        },
        spot: {
            intensity: 0.9,
            color: '#ffffbb',
            pos: [0, 0, 6],
            target: [-1, 1, 0],
            shadow: {
                debug: false
            }
        }
    });
    // add some default objects to the scene
    this.rosViewer.scene.add(new ROS3D.Grid());

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
        fixedFrame : 'map' // FIXME
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
    
    /**
     * Create an image snapshot of this canvas, create a ROS image message
     * and send that message via dedicated topic to the server.
     **/
    this.snapshot_publish = function (frameNumber, fps) {
      console.log("Publishing canvas snapshot frame:" + frameNumber + " fps:" + fps);

      // make sure the frame buffer has some content
      this.rosViewer.render();

      var gl = this.rosViewer.renderer.getContext("webgl", {preserveDrawingBuffer: true});
      var width  = gl.drawingBufferWidth;
      var height = gl.drawingBufferHeight;
      
      // Compute frame timestamp based on FPS and frame number
      var t = frameNumber/fps;
      var secs  = Math.floor(t);
      var nsecs = Math.round(1000*(t - secs));
      
      // FIXME: Why does this fail?
      //    Also it is not nice to copy the pixel data below. Would be
      //    nicer if we could use the return of glReadPixels directly.
      //var buf = new Uint8Array(width * height * 3);
      //gl.readPixels(0, 0, width, height, gl.RGB, gl.UNSIGNED_BYTE, buf);
      var buf = new Uint8Array(width * height * 4);
      gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, buf);
      // Copy to pixels array (Note: Workaround for serialization issue when using Uint8Array directly)
      var pixels = [];
      var pixelStride = 4; // 4 bytes per pixel (RGBA)
      for(var y=height-1; y>=0; y--) {
        for(var x=0; x<width; x++) {
          var index = (x + y*width)*pixelStride;
          // Read RGB, ignore alpha
          pixels.push(buf[index+0]);
          pixels.push(buf[index+1]);
          pixels.push(buf[index+2]);
        }
      }
      // Finally generate ROS message
      var msg = new ROSLIB.Message({
        header: {
          // Two-integer timestamp
          stamp: { secs:secs, nsecs:nsecs },
          // Frame this data is associated with
          frame_id: String(frameNumber),
          // Consecutively increasing ID
          seq: 0
        },
        // image height, that is, number of rows
        height: height,
        // image width, that is, number of cols
        width: width,
        // Encoding of pixels -- channel meaning, ordering, size
        encoding: "rgb8",
        // is this data bigendian?
        is_bigendian: 0,
        // Full row length in bytes
        step: width*3,
        // actual matrix data, size is (step * rows)
        data: pixels
      });
      
      that.snapshotTopic.publish(msg);
    };

    this.snapshot = function () {
        that.snapshot_publish(1,1);
        let a = document.createElement('a')
        a.href = 'userdata/snapshots/1.jpeg'
        a.download = 'snapshot.jpeg'
        document.body.appendChild(a)
        a.click()
        document.body.removeChild(a)
    };
    
    ///////////////////////////////
    //////////// ...
    ///////////////////////////////
    
    this.selectMarker = function(marker) {
        if(that.selectedMarker) {
            that.unselectMarker();
        }
        that.selectedMarker = marker;
        // inform the active iframe about selection (e.g., to show object query library)
        if(that.on_select_marker)
          that.on_select_marker(marker);
    };
    
    this.unselectMarker = function() {
      if(!that.selectedMarker)
        return;
      if(that.on_unselect_marker)
        that.on_unselect_marker(that.selectedMarker);
      that.selectedMarker = undefined;
    };
    
    this.removeMarker = function(marker) {
        if(marker === that.selectedMarker) {
            that.unselectMarker();
        }
        if(that.on_remove_marker)
            that.on_remove_marker(marker);
    };
    
    this.showMarkerMenu = function(marker) {
        if(that.on_show_marker_menu)
          that.on_show_marker_menu(marker);
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
