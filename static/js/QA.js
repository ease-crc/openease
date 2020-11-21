
/**
 * Main user interface of openEASE.
 * The ui contains a Prolog console,
 * a library of predefined queries,
 * a webgl canvas and some widgets for
 * displaying graphics and statistics.
 **/
function KnowrobUI(flask_user,options) {
    var that = this;
    
    // Blackboard object
    this.blackboard = undefined;
    // connect to ROS on host via websocket
    this.client = new ROSClient({
        flask_user:     flask_user,
        ros_url:        options.ros_url,
        authentication: options.authentication,
        auth_url: '/api/v1.0/auth_by_session'
    });
    // Neem id 
    this.neem_id = options.neem_id || '5f22b1f512db5aed7cd1961a';
    // query id of most recent query
    this.last_qid = undefined;
    this.last_query = undefined;
    // a console used to send queries to KnowRob
    this.console = new PrologConsole(that.client, {
        query_div: 'user_query',
        on_query: function(qid,q) {
            that.last_qid = qid;
            that.last_query = q;
            //
            var ace_edit = ace.edit("input-text");
            ace_edit.setValue(q);
            ace_edit.clearSelection();
            $('#input').collapse('show');
            //
            that.initBlackboard();
            $('#blackboard-container').collapse('show');
            $('#btn_query_next').collapse('hide');
            $('.query-icon').removeClass('fa-question').addClass('fa-spinner fa-spin');
        },
        on_query_answer: function(qid,answer) {
            that.blackboard.addQueryResponse(that.console, answer);
            // run a query to generate visualization messages
            var bindings=[];
            for (var key in answer.solution) {
                bindings.push([key,answer.solution[key]]);
            }
            const pl = new ROSPrologClient(that.client.ros, {});
            const query_string = "openease_query(("+that.last_query+"),"+JSON.stringify(bindings)+")";
            pl.jsonQuery(query_string, function(result) {
                pl.finishClient();
                $('.query-icon').removeClass('fa-spinner fa-spin').addClass('fa-question');
                $('#btn_query_next').collapse('show');
            });
        },
        on_query_finish: function(qid) {
            if(qid==that.last_qid) {
                $('#btn_query_next').collapse('hide');
                that.blackboard.finish();
            }
        }
    });
    // the 3D visualization canvas
    this.canvas = undefined;
    this.canvas_div = $("<div>");
    this.canvas_div.attr("id", "markers");
    this.canvas_div.addClass("row");
    this.playback = undefined;

    this.nextSolution = function () {
        that.initBlackboard();
        that.console.nextSolution();
    };

    this.init = function () {
        that.console.init();
        that.setupInputWidget();
        that.client.connect(function(ros) {
            console.info('Connected to ROS.');
            that.registerROSClients(ros);
            //that.rosViewer.registerNodes(ros);
        });
    };

    this.initBlackboard = function () {
        if(that.blackboard) {
            that.blackboard.delete();
        }
        that.blackboard = new Blackboard($("#blackboard"),
            that.last_qid, that.last_query);
    };
    
    // create a ROSCanvas in the "markers" div.
    this.getCanvas = function() {
        if(!that.canvas) {
            that.canvas = new ROSCanvas({
                parent: that.canvas_div,
                // meshPath is the prefix for GET requests
                meshPath: '/meshes/'
            });
            that.canvas.registerNodes(that.client.ros);
        }
        return that.canvas;
    };
    /*
    this.initCanvas = function() {
        if(that.rosViewer) {
          delete that.rosViewer;
          document.getElementById('markers').innerHTML = "";
        }
        that.rosViewer = new ROSCanvas({
            parent: document.getElementById('markers'),
            // meshPath is the prefix for GET requests
            meshPath: '/meshes/'
        });
        that.rosViewer.on_camera_pose_received = that.setCameraPose;
        that.rosViewer.on_unselect_marker = function(marker) {
            that.rosViewer.rosViewer.unhighlight(marker);
        };
        that.rosViewer.on_select_marker = function(marker) {
            that.rosViewer.rosViewer.highlight(marker);
        };
        that.rosViewer.on_window_dblclick = function() {
            if(that.rosViewer.selectedMarker) {
                that.rosViewer.unselectMarker();
            }
        };
    };
    */
    
    // listen to some ROS topics
    this.registerROSClients = function (ros) {
        that.registerChartClient(ros);
        that.registerImageClient(ros);
        that.registerMarkerClient(ros);
        that.registerTickClient(ros);
        that.waitForProlog(ros, function() {
            console.info('Connected to KnowRob.');
            const pl = new ROSPrologClient(ros, {});
            pl.jsonQuery("neem_init('" + that.neem_id + "').", function(result) {
                console.info("NEEM has been initialized");
                pl.finishClient();

                if(options.has_query=='True') {
                    that.console.query();
                }
                else {
                    $('.query-icon').removeClass('fa-spinner fa-spin').addClass('fa-question');
                }
            });
        });
    };
    
    this.waitForProlog = function (ros, then) {
        if(!ros) return;
        const pl = new ROSPrologClient(ros, {});
        if(!pl) return;
        pl.jsonQuery("true", function(result) {
            pl.finishClient();
            if(result.error) {
                setTimeout(function() { that.waitForProlog(ros, then) }, 500);
            }
            else {
                then();
            }
        });
    };

    // listen to republish_tick topic
    this.registerTickClient = function(ros) {
        that.tickClient = new ROSLIB.Topic({
            ros : ros,
            name : 'republisher_tick',
            messageType : 'std_msgs/Float64'
        });
        that.tickClient.subscribe(function(time) {
            if(that.playback) {
                that.playback.tick(time.data);
            }
            if(that.blackboard) {
                that.blackboard.tick(time.data);
            }
        });
    };
    
    // listen to data_vis_msgs topic and add charts to currently
    // active query card
    this.registerChartClient = function(ros) {
        that.dataVis = new ROSLIB.Topic({
            ros : ros,
            name : 'data_vis_msgs',
            messageType : 'data_vis_msgs/DataVis'
        });
        that.dataVis.subscribe(function(data_vis_msg) {
            if(that.last_qid) {
                if(data_vis_msg.type == 989) {
                    that.playback = new PlaybackWidget(that.canvas_div, {
                        event: data_vis_msg.values[0].value1[0],
                        time_min: parseFloat(data_vis_msg.values[0].value2[0]),
                        time_max: parseFloat(data_vis_msg.values[0].value2[1]),
                        ros: ros
                    });
                    that.blackboard.push('Scene',
                        that.blackboard.createItem(that.playback.getWidget(), {
                            border: false
                        }));
                    that.getCanvas().resize();
                }
                else {
                    that.blackboard.addChart(data_vis_msg);
                }
            }
            else {
                console.warn("Received DataVis msg, but no query is active.");
            }
        });
    };
    
    // listen to logged_images topic and add images to currently
    // active query card
    this.registerImageClient = function(ros) {
        that.imageVis = new ROSLIB.Topic({
            ros : ros,
            name : 'logged_images',
            messageType : 'std_msgs/String'
        });
        that.imageVis.subscribe(function(image_uri) {
            if(that.last_qid) {
                const ext = image_uri.data.substr(
                    image_uri.data.lastIndexOf('.') + 1).toLowerCase();
                if(ext==='jpg' || ext ==='png') {
                    that.blackboard.addImage("Images", image_uri);
                }
                else if(ext ==='ogg' || ext ==='ogv' || ext ==='mp4' || ext ==='mov') {
                    that.blackboard.addVideo("Videos", image_uri);
                }
                else {
                    console.warn("Unknown data format on /logged_images topic: " + image_uri.data);
                }
            }
            else {
                console.warn("Received logged_image msg, but no query is active.");
            }
        });
    };

    this.registerMarkerClient = function(ros) {
        that.markerVis = new ROSLIB.Topic({
            ros : ros,
            name : '/visualization_marker_array',
            messageType : 'visualization_msgs/MarkerArray',
            compression : 'png'
        });
        that.markerVis.subscribe(function(marker_msg) {
            if(that.blackboard) {
                that.getCanvas().addMarkerArray(marker_msg);
            }
        });
    };
    
//     this.registerHighlightClient = function() {
//         that.highlightClient = new ROSLIB.Topic({
//             ros : that.client.ros,
//             name : '/openease/highlight',
//             messageType : 'knowrob_openease/Highlight'
//         });
//         that.highlightClient.subscribe(function(msg) {
//           var r = msg.color.r;
//           var g = msg.color.g;
//           var b = msg.color.b;
//           var a = msg.color.a;
//           for(var i in msg.objects) {
//               var o = msg.objects[i];
//               var marker = that.client.markerArrayClient.getObjectMarker(o.toString());
//               if(!marker) continue;
//               if(r>0 || g>0 || b>0 || a>0) {
//                   that.rosViewer.highlight(marker, [r,g,b,a]);
//               } else {
//                   that.rosViewer.unhighlight(marker);
//               }
//           }
//         });
//     };

    this.setupInputWidget = function () {
        // Use ace editor for stylized display
        // of query string.
        var ace_edit = ace.edit("input-text");
        //ace_edit.setTheme("ace/theme/solarized_light");
        ace_edit.getSession().setMode("ace/mode/prolog");
        ace_edit.setOptions({
            // editor options
            selectionStyle: "text",
            highlightActiveLine: false,
            highlightSelectedWord: false,
            readOnly: true,
            cursorStyle: "slim",
            behavioursEnabled: false,
            wrapBehavioursEnabled: false,
            autoScrollEditorIntoView: false,
            enableMultiselect: false,
            // renderer options
            highlightGutterLine: false,
            maxLines: 15,
            showGutter: false,
            showLineNumbers: false,
            showFoldWidgets: false,
            printMarginColumn: false,
            showPrintMargin: false
        });
        ace_edit.setShowPrintMargin(false);
        ace_edit.setOption("showPrintMargin", false);
        ace_edit.renderer.setScrollMargin(6, 6, 6, 6);
    };
}
