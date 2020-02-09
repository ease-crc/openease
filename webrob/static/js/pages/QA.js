/**
 * Main user interface of openEASE.
 * The ui contains a Prolog console,
 * a library of predefined queries,
 * a webgl canvas and some widgets for
 * displaying graphics and statistics.
 **/
function KnowrobUI(flask_user,options) {
    var that = this;
    
    this.imageWidth = function(doc) { return 0.0; };
    this.imageHeight = function(doc) { return 0.0; };
    
    this.client = new ROSClient({
        flask_user:     flask_user,
        ros_url:        options.ros_url,
        authentication: options.authentication,
        auth_url: '/api/v1.0/auth_by_session'
    });
    this.console = new PrologConsole(that.client, options);
    
    this.rosViewer = undefined;
    this.queryLibrary = undefined;
    this.chartVisClient = undefined;
    this.imageClient = undefined;

    this.init = function () {
        that.console.init();
        that.initCanvas();
        that.client.connect(function(ros) {
            that.registerROSClients();
            that.rosViewer.registerNodes(ros);
            that.initQueryLibrary();
        });
        
        that.resizeCanvas();
        $('#mjpeg').resize(function(){
            var timeout = function(){ if(that.resizeImage()) window.setTimeout(timeout, 10); };
            if(that.resizeImage()) window.setTimeout(timeout, 10);
        });
    };
    
    this.initCanvas = function() {
        if(that.rosViewer) {
          delete that.rosViewer;
          document.getElementById('markers').innerHTML = "";
        }
        that.rosViewer = new ROSCanvas({
            divID: document.getElementById('markers'),
            meshPath: '/meshes/'
        });
        that.rosViewer.on_camera_pose_received = that.setCameraPose;
        that.rosViewer.on_unselect_marker = function(marker) {
            that.rosViewer.unhighlight(marker);
            that.initQueryLibrary(marker);
        };
        that.rosViewer.on_select_marker = function(marker) {
            that.rosViewer.highlight(marker);
            that.loadMarkerQueries(marker);
        };
        that.rosViewer.on_remove_marker = function(marker) {
            if(marker === that.rosViewer.selectedMarker) {
                that.initQueryLibrary();
            }
        };
        that.rosViewer.on_window_dblclick = function() {
            if(that.rosViewer.selectedMarker) {
                that.initQueryLibrary();
                that.rosViewer.unselectMarker();
            }
        };
    };

    this.resizeImage = function () {
        return imageResizer($('#mjpeg_image'),
                            $('#mjpeg'),
                            that.imageWidth($('#mjpeg_image')[0]),
                            that.imageHeight($('#mjpeg_image')[0])
                           );
    };

    this.resizeCanvas = function () {
        that.rosViewer.resize($('#markers').width(), $('#markers').height());
    };
    
    this.setCameraPose = function (pose) {
        that.rosViewer.setCameraPose(pose);
    };
    
    this.registerROSClients = function () {
        that.chartVisClient = new DataVisClient({
            ros: that.client.ros,
            containerId: '#chart',
            topic: 'data_vis_msgs'
        });
        // Setup the image message client.
        that.imageClient = new ROSLIB.Topic({
          ros : that.client.ros,
          name : '/logged_images',
          messageType : 'std_msgs/String'
        });
        that.imageClient.subscribe(function(message) {
            var ext = message.data.substr(message.data.lastIndexOf('.') + 1).toLowerCase();
            var url = message.data;
            if(!url.startsWith("/knowrob/")) {
                 if(url.startsWith("/home/ros/user_data"))  url = '/user_data/'+url.replace("/home/ros/user_data/", "");
                 else url = '/knowrob/knowrob_data/'+url;
            }
            var imageHeight, imageWidth;
            var html = '';
            if(ext=='jpg' || ext =='png') {
                html += '<div class="image_view">';
                html += '<img id="mjpeg_image" class="picture" src="'+url+'" width="300" height="240"/>';
                html += '</div>';
                
                imageHeight = function(mjpeg_image) { return mjpeg_image.height; };
                imageWidth  = function(mjpeg_image) { return mjpeg_image.width; };
            }
            else if(ext =='ogg' || ext =='ogv' || ext =='mp4' || ext =='mov') {
                html += '<div class="image_view">';
                html += '  <video id="mjpeg_image" controls autoplay loop>';
                html += '    <source src="'+url+'" ';
                if(ext =='ogg' || ext =='ogv') html += 'type="video/ogg" ';
                else if(ext =='mp4') html += 'type="video/mp4" ';
                html += '/>';
                html += 'Your browser does not support the video tag.';
                html += '</video></div>';
                
                imageHeight = function(mjpeg_image) { return mjpeg_image.videoHeight; };
                imageWidth  = function(mjpeg_image) { return mjpeg_image.videoWidth; };
            }
            else {
                console.warn("Unknown data format on /logged_images topic: " + message.data);
            }
            if(html.length>0 && that.on_image_received) {
                that.on_image_received(html, imageWidth, imageHeight);
            }
        });
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
    };
    
    this.on_image_received = function(imageHtml, imageWidth, imageHeight) {
//         that.imageWidth = imageWidth;
//         that.imageHeight = imageHeight;
        document.getElementById('mjpeg').innerHTML = imageHtml;
        $('#mjpeg').change();
        $('#mjpeg').resize();
    };
    
    ///////////////////////////////
    //////////// Query Library
    ///////////////////////////////
    
    this.loadQueriesForObject = function(objectName) {
        var prolog = new ROSPrologClient(that.client.ros, {});
        prolog.jsonQuery("object_queries("+objectName+",Queries).",
            function(result) {
                prolog.finishClient();
                that.loadObjectQueries(result.solution.Queries);
            }
        );
    };
    
    this.loadObjectQueries = function(queries) {
        // parse query and add to category--queries map
        var queryLibMap = {};
        for(var i=0; i<queries.length; i++) {
            var category = queries[i][0];
            var title = queries[i][1];
            var query = queries[i][2];
            if(!query.endsWith(".")) query += ".";
            
            if(queryLibMap[category]==undefined) queryLibMap[category]=[];
            queryLibMap[category].push({q: query, text: title});
        }
        // flatten the map into queryLib array
        var queryLib = [];
        var categories = Object.keys(queryLibMap);
        categories.sort();
        for(var i=0; i<categories.length; i++) {
            queryLib.push({q: "", text: "----- " + categories[i] + " -----"});
            queryLib.push.apply(queryLib, queryLibMap[categories[i]]);
        }
        
        that.initQueryLibrary(queryLib);
    };
    
    this.loadMarkerQueries = function(marker) {
        var prolog = new ROSPrologClient(that.client.ros, {});
        prolog.jsonQuery("term_to_atom("+marker.ns+",MarkerName), "+
            "marker_queries(MarkerName, MarkerQueries).",
            function(result) {
                prolog.finishClient();
                that.loadObjectQueries(result.solution.MarkerQueries);
            }
        );
    };
    
    this.initQueryLibrary = function (queries) {
        function loadQueries(query_lib) {
            var lib_div = document.getElementById('library_content');
            if(lib_div !== null && query_lib) {
                lib_div.innerHTML = '';
                
                var query = query_lib.query || query_lib;
                for (var i = 0; i < query.length; i++) {
                    if(!query[i].text) continue;

                    var text = query[i].text.trim();
                    if(text.length==0) continue;
                    
                    if(text.startsWith('-----')) {
                        // insert space between sections
                        if(i>0) {
                            var x = document.createElement('div');
                            x.className = 'query_lib_space';
                            lib_div.appendChild(x);
                        }
                        
                        var x = document.createElement('div');
                        x.className = 'query_lib_header';
                        x.innerHTML = text.split("-----")[1].trim();
                        lib_div.appendChild(x);
                    }
                    else if(query[i].q) {
                        var x = document.createElement('button');
                        x.type = 'button';
                        x.value = query[i].q;
                        x.className = 'query_lib_button';
                        x.innerHTML = text;
                        lib_div.appendChild(x);
                    }
                }
                that.queryLibrary = query;
            }
            
            $( "button.query_lib_button" )
                .focus(function( ) {
                    that.console.setQueryValue( $(this)['0'].value );
                });
        };
        
        if(queries == undefined) {
          // TODO: only dowload if required!
            // FIXME
          //that.client.episode.queryEpisodeData(loadQueries);
        }
        else {
            loadQueries(queries);
        }
    };
    
    $("#library_content").keydown(function(e) {
        var button = $(".query_lib_button:focus");
        if (e.keyCode == 40) { // down
            for(var next=button.next(); next.length>0; next=next.next()) {
                if(next.hasClass('query_lib_button')) {
                    next.focus();
                    next.click();
                    break;
                }
            }
            e.preventDefault();
        }
        else if (e.keyCode == 38) { // up
            for(var prev=button.prev(); prev.length>0; prev=prev.prev()) {
                if(prev.hasClass('query_lib_button')) {
                    prev.focus();
                    prev.click();
                    break;
                }
            }
            e.preventDefault();
        }
        else if (e.keyCode == 32) { // space
            e.preventDefault();
            that.console.query();
        }
    });
};
