
/**
 * Main user interface of openEASE.
 * The ui contains a Prolog console,
 * a library of predefined queries,
 * a webgl canvas and some widgets for
 * displaying graphics and statistics.
 **/
function KnowrobUI(flask_user,options) {
    let that = this;
    
    // qid-QueryCard map
    this.queryCards = {};
    // connect to ROS on host via websocket
    this.client = new ROSClient({
        flask_user:     flask_user,
        ros_url:        options.ros_url,
        authentication: options.authentication,
        auth_url: '/api/v1.0/auth_by_session'
    });
    // Neem id 
    this.neem_id = '';
    // query id of most recent query
    this.last_qid = undefined;
    // a console used to send queries to KnowRob
    this.console = new PrologConsole(that.client, {
        query_div: 'user_query',
        neem_id: options.neem_id,
        on_query: function(qid,q) {
            $('#btn_query_next').prop('disabled', false);
            const last_query_card = that.queryCards[that.last_qid];
            if(last_query_card) {
                last_query_card.collapse();
            }
            that.last_qid = qid;
            that.queryCards[qid] = new QueryCard("#history",qid,q);
        },
        on_query_answer: function(qid,answer) {
            that.queryCards[qid].addQueryResponse(answer);
        },
        on_query_finish: function(qid) {
            $('#btn_query_next').prop('disabled', true);
            that.queryCards[qid].finish();
        }
    });
    // the 3D visualization canvas
    this.rosViewer = undefined;

    this.init = function () {
        that.console.init();
        that.initCanvas();
        that.client.connect(function(ros) {
            that.registerROSClients(ros);
            that.rosViewer.registerNodes(ros);
        });
        that.resizeCanvas();
    };
    
    // create a ROSCanvas in the "markers" div.
    this.initCanvas = function() {
        if(that.rosViewer) {
          delete that.rosViewer;
          document.getElementById('markers').innerHTML = "";
        }
        that.rosViewer = new ROSCanvas({
            divID: document.getElementById('markers'),
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
    
    // listen to some ROS topics
    this.registerROSClients = function (ros) {
        that.registerChartClient(ros);
        that.registerImageClient(ros);
    };
    
    this.waitForProlog = function (ros, then) {
        const pl = new ROSPrologClient(ros, {});
        if(!pl) return;
        pl.jsonQuery("true", function(result) {
            pl.finishClient();
            if(result.error) {
                setTimeout(that.waitForProlog, 500);
            }
            else {
                then();
            }
        });
    };

    this.resizeCanvas = function () {
        const markers = $('#markers');
        that.rosViewer.resize(markers.width(), markers.height());
    };
    this.setCameraPose = function (pose) {
        that.rosViewer.setCameraPose(pose);
    };
    this.snapshot = function () {
        that.rosViewer.snapshot(1,1 );
        let a = document.createElement('a')
        a.href = 'userdata/snapshots/1.jpeg'
        a.download = 'snapshot.jpeg'
        document.body.appendChild(a)
        a.click()
        document.body.removeChild(a)
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
                that.queryCards[that.last_qid].addChart(data_vis_msg);
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
                    that.queryCards[that.last_qid].addImage(image_uri);
                }
                else if(ext ==='ogg' || ext ==='ogv' || ext ==='mp4' || ext ==='mov') {
                    that.queryCards[that.last_qid].addVideo(image_uri);
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
}
