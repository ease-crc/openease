
/**
 * WebGL canvas for marker visualization.
 **/
function ROSCanvas(options){
    var that = this;
    
    // Prefix for mesh GET URL's
    var meshPath  = options.meshPath || '/';
    // sprite markers and render events
    var sprites = [];
    var render_event;
    // ROS messages
    this.tfClient = undefined;
    this.cameraPoseClient = undefined;
    this.snapshotTopic = undefined;
    this.markerArrayClient = undefined;
    // The selected marker object or undefined
    this.selectedMarker = undefined;
    
    // The canvas object
    this.rosViewer = new EASEViewer({
        client: that,
        div : options.divID,
        width : 1920,
        height : 1080,
        antialias : false,
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
    
    this.resize = function (w,h) {
      // update perspective projection
      this.rosViewer.resize(w, h);
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
        transThres : 0.01,
        rate : 10.0,
        fixedFrame : 'map' // FIXME
      });
      // Setup the marker array client.
      that.markerArrayClient = new MarkerArrayClient({
        ros : ros,
        tfClient : that.tfClient,
        topic : '/visualization_marker_array',
        canvas : that,
        path : meshPath
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
    
    /**
     * Create an image snapshot of this canvas, create a ROS image message
     * and send that message via dedicated topic to the server.
     **/
    this.snapshot = function (frameNumber, fps) {
      console.log("Publishing canvas snapshot frame:" + frameNumber + " fps:" + fps);
      
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
    
    ///////////////////////////////
    //////////// Frame Overlay
    ///////////////////////////////
    
//     this.createOverlay = function() {
//         // Create page iosOverlay
//         var page = document.getElementById('page');
//         if(page) {
//             var pageOverlay = document.createElement("div");
//             pageOverlay.setAttribute("id", "page-overlay");
//             pageOverlay.className = "ios-overlay ios-overlay-hide div-overlay";
//             pageOverlay.innerHTML += '<span class="title"></span';
//             pageOverlay.style.display = 'none';
//             page.appendChild(pageOverlay);
//             var spinner = createSpinner();
//             pageOverlay.appendChild(spinner.el);
//         }
//     };
//     
//     this.showPageOverlay = function(text) {
//       var pageOverlay = document.getElementById('page-overlay');
//       if(pageOverlay && !that.pageOverlayDisabled) {
//           pageOverlay.children[0].innerHTML = text;
//           pageOverlay.style.display = 'block';
//           pageOverlay.className = pageOverlay.className.replace("hide","show");
//           pageOverlay.style.pointerEvents = "auto";
//           that.pageOverlayDisabled = true;
//       }
//     };
//     
//     this.hidePageOverlay = function() {
//       var pageOverlay = document.getElementById('page-overlay');
//       if(pageOverlay && that.pageOverlayDisabled) {
//           //pageOverlay.style.display = 'none';
//           pageOverlay.className = pageOverlay.className.replace("show","hide");
//           pageOverlay.style.pointerEvents = "none";
//           that.pageOverlayDisabled = false;
//       }
//     };
};
