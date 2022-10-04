
/**
 * Main user interface of openEASE.
 * The ui contains a Prolog console,
 * a library of predefined queries,
 * a webgl canvas and some widgets for
 * displaying graphics and statistics.
 **/
function KnowrobUI(flask_user,ros_client,options) {
    var that = this;
    
    // Blackboard object
    this.blackboard = undefined;
    // connect to ROS on host via websocket
    this.client = ros_client;
    // Neem id 
    this.neem_id = options.neem_id || '5f22b1f512db5aed7cd1961a';
    // query id of most recent query
    this.last_qid = undefined;
    this.last_query = undefined;
    this.formatter = new EntityFormatter();
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
            $('#follow-up-question').collapse('hide');
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
            const query_string = "openease_query('"+ that.last_qid + "'," +
                "("+that.last_query+"),"+JSON.stringify(bindings)+")";
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
    };

    this.setStatus = function(msg) {
        $('#query_status').text(msg);
    };

    this.clearStatus = function() {
        this.setStatus('');
    };

    this.initBlackboard = function () {
        if(that.canvas) {
            that.canvas.clearHiglights();
        }
        if(that.blackboard) {
            that.blackboard.delete();
        }
        that.blackboard = new Blackboard({
            where: $("#blackboard"),
            qid: that.last_qid,
            query_string: that.last_query,
            formatter: that.formatter
        });
    };
    
    // create a ROSCanvas in the "markers" div.
    this.getCanvas = function() {
        if(!that.canvas) {
            that.canvas = new ROSCanvas({
                parent: that.canvas_div,
                // meshPath is the prefix for GET requests
                meshPath: '/meshes/',
                on_start_loading: function() {
                    // meshes downloading started
                    that.setStatus('Loading meshes');
                    $('.query-icon').removeClass('fa-question').addClass('fa-spinner fa-spin');
                },
                on_finished_loading: function() {
                    // meshes downloading finished
                    that.clearStatus();
                    $('.query-icon').removeClass('fa-spinner fa-spin').addClass('fa-question');
                }
            });
            that.canvas.registerNodes(that.client.ros);
        }
        return that.canvas;
    };
    /*
    this.initCanvas = function() {
        that.rosViewer.on_camera_pose_received = that.setCameraPose;
        that.rosViewer.on_window_dblclick = function() {
            if(that.rosViewer.selectedMarker) {
                that.rosViewer.unselectMarker();
            }
        };
    };
    */
    
    // listen to some ROS topics
    this.onconnect = function (ros) {
        that.ros = ros;
        that.registerChartClient(ros);
        that.registerImageClient(ros);
        that.registerMarkerClient(ros);
        that.registerTickClient(ros);
        that.setStatus('Connecting to knowledge base');
        waitForProlog(ros, function() {
            console.info('Connected to KnowRob.');
            that.initNEEM(ros, function() {
                that.formatter.connect(ros, function() {
                    that.clearStatus();
                    if(options.has_query=='True') {
                        that.console.query();
                    }
                    else {
                        $('.query-icon').removeClass('fa-spinner fa-spin').addClass('fa-question');
                    }
                });
            });
        });
    };

    this.initNEEM = function (ros,then) {
        if(options.load_neem=='True') {
            const pl = new ROSPrologClient(ros, {});
            that.setStatus('Loading NEEM');
            pl.jsonQuery("register_ros_package(openease_rules), knowrob_load_neem('" + that.neem_id + "').", function(result) {
                pl.finishClient();
                console.info("NEEM has been initialized");
                if(then) {
                    then();
                }
            });
        } else {
            const pl = new ROSPrologClient(ros, {});
            pl.jsonQuery("register_ros_package(openease_rules).", function(result) {
                pl.finishClient();
                if(then) {
                    then();
                }
            });
        }
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
            if(that.last_qid === data_vis_msg.id) {
                if(data_vis_msg.type == 989) {
                    that.playback = new PlaybackWidget(that.canvas_div, {
                        event: data_vis_msg.values[0].value1[0],
                        time_min: parseFloat(data_vis_msg.values[0].value2[0]),
                        time_max: parseFloat(data_vis_msg.values[0].value2[1]),
                        ros: ros
                    });
                    that.blackboard.push('Replay of event',
                        that.blackboard.createItem(that.playback.getWidget(), {
                            border: false
                        }));
                    that.getCanvas().resize();
                    // FIXME: not sure why but doing this in the constructor of ROSCanvas causes
                    //         that click function is not called after hitting next
                    that.getCanvas().link_screenshot.click(
                        function() {that.getCanvas().snapshot()});
                    that.getCanvas().link_maximize.click(
                        function() {that.getCanvas().maximize()});
                } 
                else if(data_vis_msg.type == 988) {
                    that.blackboard.addMesh(that.client.ros, data_vis_msg);
                }
                else if(data_vis_msg.type == 100) {
                    that.blackboard.addResultDescription(that.console, data_vis_msg.id, data_vis_msg.values)
                }
                else if(data_vis_msg.type == 977) {
                    var frameArray = data_vis_msg.values[0].value1;
                    for(var i in frameArray) {
                        that.getCanvas().selectFrame(frameArray[i]);
                        console.info(frameArray[i]);
                    } 
                }
                else {
                    that.blackboard.addChart(data_vis_msg);
                }
            }
            else if(!that.last_qid) {
                console.warn("Received DataVis msg, but no query is active.");
            }
            else {
                console.warn("Received DataVis msg with ID \"" + data_vis_msg.id + "\", but the associated query is not active (anymore). The active query has the ID \"" + that.last_qid + "\".");
                console.warn(data_vis_msg);
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
            that.getCanvas().addMarkerArray(marker_msg);
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
