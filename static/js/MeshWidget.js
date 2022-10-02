/**
 * A widget showing a description of the bindings of a KnowRob query.
 *
 * @param options
 * @constructor
 */
function MeshWidget(options){
    const that = this;
    options = options || {};
    this.path = options.path || '/';
    this.tfPrefix = options.tfPrefix || '';
    this.loader = options.loader;
    this.ros = options.ros;
    this.has_failure = false;
    
    var aspect_ratio = 16.0/9.0;
    var canvas_width = 400;
    var canvas_height = canvas_width/aspect_ratio;
    var is_maximized = false;

    this.title = "Object mesh";
    this.widget = $("<div>");
    this.widget.addClass("ease-border card-body bg-light");
    
    this.container = $("<div>");
    this.container.addClass("ease-border m-auto mesh-widget");
    this.container.uniqueId();
    this.widget.append(this.container);
    
    // Setup a client to listen to TFs.
    // note thaturdf will be shown in initial pose if no one publishes.
    this.tfClient = new ROSLIB.TFClient({
      ros : that.ros,
      angularThres : 0.01,
      transThres : 0.01,
      rate : 10.0
    });
    
    // Create the main viewer.
    this.viewer = new ROS3D.Viewer({
        background : '#ffffff',
        cameraPose : {x : 1, y : 1, z : 1 },
        elem : { appendChild: function(x){ that.container.append(x); } },
        width : canvas_width,
        height : canvas_height,
        antialias : true
    });
    this.viewer.cameraControls.autoRotate = true;
    // Add a grid.
    this.viewer.scene.add(new ROS3D.Grid({
        color: '#cccccc',
        num_cells: 20,
        lineWidth: 1,
        cellSize: 1
    }));

    this.get = function() {
        return that.widget;
    };

    this.addMesh = function(mesh_id, uri) {
        // strips package://
        var tmpIndex = uri.indexOf('package://');
        if (tmpIndex !== -1) {
            uri = uri.substr(tmpIndex + ('package://').length);
        }
        var fileType = uri.substr(-3).toLowerCase();
        //
        if (ROS3D.MeshLoader.loaders[fileType]) {
            var mesh = new ROS3D.MeshResource({
                resource : uri,
                loader : that.loader,
                //material : colorMaterial,
                path : that.path
            });
            var sceneNode = new ROS3D.SceneNode({
                frameID : mesh_id,
                object : mesh,
                //pose : visual.origin,
                pose : new ROSLIB.Pose(),
                tfClient : that.tfClient
            });
            //sceneNode.name = visual.name;
            that.viewer.scene.add(sceneNode);
        }
        else {
            that.has_failure = true;
        }
    };

    this.addURDF = function(urdfFile) {
        var urdfModel = new ROSLIB.UrdfModel({
            string : urdfFile
        });
        that.urdf = new ROS3D.Urdf({
            urdfModel : urdfModel,
            path : that.path,
            tfClient : that.tfClient,
            tfPrefix : that.tfPrefix,
            loader : that.loader
        });
        that.viewer.scene.add(that.urdf);
    };

    this.resize = function () {
        // compute canvas size
        //const max_width = options.parent.parent().parent().width()-4;
        const small_width = 400;
        //if(!is_maximized && small_width<max_width) {
            canvas_width = 400;
        //}
        //else {
        //    canvas_width = max_width;
        //}
        canvas_height = canvas_width/aspect_ratio;
        // resize
        that.viewer.camera.width = canvas_width;
        that.viewer.camera.height = canvas_height;
        that.viewer.camera.aspect = canvas_width / canvas_height;
        that.viewer.camera.updateProjectionMatrix();
        that.viewer.renderer.setSize(canvas_width, canvas_height);
        //that.viewer.composer.setSize(canvas_width, canvas_height);
    };
    $(window).on('resize', that.resize);
}

