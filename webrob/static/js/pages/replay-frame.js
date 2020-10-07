
function formatDate(unix_timestamp) {
    // create a new javascript Date object based on the timestamp
    // multiplied by 1000 so that the argument is in milliseconds, not seconds
    var date = new Date(unix_timestamp*1000);
    // hours part from the timestamp
    var hours = date.getHours();
    // minutes part from the timestamp
    var minutes = "0" + date.getMinutes();
    // seconds part from the timestamp
    var seconds = "0" + date.getSeconds();
    // will display time in 10:30:23 format
    return hours + ':' + minutes.substr(minutes.length-2) + ':' + seconds.substr(seconds.length-2);
};

/**
 * Episode replay user interface of openEASE.
 **/
function KnowrobReplayUI(flask_user,options) {
    var that = this;

    // connect to ROS on host via websocket
    this.client = new ROSClient({
        flask_user:     flask_user,
        ros_url:        options.ros_url,
        authentication: options.authentication,
        auth_url: '/api/v1.0/auth_by_session'
    });
    // the 3D visualization canvas
    this.rosViewer = undefined;
    
    var isStreaming = false;
    var isNewVideo = false;

    this.init = function () {
        that.rosViewer = new ROSCanvas({
            divID: document.getElementById('markers'),
            // meshPath is the prefix for GET requests
            meshPath: '/meshes/'
        });
        that.setupVideoQuery();
        that.updateStartTime();
        that.updateEndTime();
        that.newVideo();
        that.resizeCanvas();
    };

    this.resizeCanvas = function () {
        that.rosViewer.resize($('#markers').width(), $('#markers').height());
    };

    this.setCameraPose = function (pose) {
        that.rosViewer.setCameraPose(pose);
    };
    
    this.setupVideoQuery = function() {
        var userQuery = ace.edit('user_query');
        userQuery.resize(true);
        userQuery.setTheme("ace/theme/solarized_light");
        userQuery.getSession().setMode("ace/mode/prolog");
        userQuery.getSession().setUseWrapMode(true);
        userQuery.setOptions({
            showGutter: false,
            printMarginColumn: false,
            highlightActiveLine: false,
            highlightGutterLine: false,
            enableBasicAutocompletion: true
        });
        
        var initQuery = ace.edit('init_query');
        initQuery.resize(true);
        initQuery.setTheme("ace/theme/solarized_light");
        initQuery.getSession().setMode("ace/mode/prolog");
        initQuery.getSession().setUseWrapMode(true);
        initQuery.setOptions({
            showGutter: false,
            printMarginColumn: false,
            highlightActiveLine: false,
            highlightGutterLine: false,
            enableBasicAutocompletion: true
        });
    };
    
    this.updateStartTime = function() {
        document.getElementById("replay-start-value").innerHTML =
            formatDate(document.getElementById("replay-start-time").value);
    };
    this.updateEndTime = function() {
        document.getElementById("replay-end-value").innerHTML =
            formatDate(document.getElementById("replay-end-time").value);
    };
    
    this.formatInitialQuery = function(t) {
        var fps = parseInt(document.getElementById("replay-fps").value, 10);
        var init_query = "openease_video_fps(" + fps +
            "), openease_video_start, !, openease_clear_canvas, T = " + t + ", " +
            ace.edit('init_query').getValue().trim();
        init_query = init_query.substr(0, init_query.length - 1);
        return init_query;
    };
    
    this.stopStreaming = function() {
        if(isStreaming) {
           var prolog = that.client.newProlog();
           prolog.jsonQuery('openease_video_stop.', function(result) {
              window.open('/knowrob/local_data/video_created/video.mp4','_blank')
           }, mode=1);
        }
        isStreaming = false;
        var toggleButton = document.getElementById("replay-toggle-button");
        toggleButton.onclick = that.startStreaming;
        toggleButton.innerHTML = "Start";
    };
    
    this.startStreaming = function() {
        var toggleButton = document.getElementById("replay-toggle-button");
        toggleButton.onclick = that.stopStreaming;
        toggleButton.innerHTML = "Stop";
        var t0  = parseInt(document.getElementById("replay-start-time").value, 10);
        var t1  = parseInt(document.getElementById("replay-end-time").value, 10);
        // run initial query
        var init_query = that.formatInitialQuery(t0.toString());
        var prolog = that.client.newProlog();
        prolog.jsonQuery(init_query, function(result) {
            // then start streaming
            setTimeout(function(){ that.streamRange(t0,t1); }, 500);
        }, mode=1);
    };
    
    this.streamRange = function(t0, t1) {
        var video_query = ace.edit('user_query').getValue();
        var fps = parseInt(document.getElementById("replay-fps").value, 10);
        var step_sec = 1.0/fps;
        var frame_number = 1;
        var t = t0;
        isStreaming = true;
        
        function streamStep(){
          if(!isStreaming) return;
          t += step_sec;
          if(t>t1) return;
          that.updateProgressBar(t0, t1, t);
          
          var query = "T = " + t.toString() + ", " + video_query;
          query = query.trim();
          query = query.substr(0, query.length - 1);
          
          var prolog = that.client.newProlog();
          prolog.jsonQuery(query, function(result) {
            that.renderHUD(t, function(){
              that.rosViewer.snapshot(frame_number, fps);
              frame_number += 1;
              streamStep();
            });
          }, mode=1);
        };
        streamStep();
    };
    
    this.updateProgressBar = function(t0, t1, t) {
        document.getElementById("replay-progress-bar").value = 100*(t-t0)/(t1-t0);
        document.getElementById("replay-progress-value").innerHTML = formatDate(t);
    }
    
    this.renderHUD = function(t, handler) {
        var timeString = "Time: " + formatDate(t);
        // TODO: use proper query to update the canvas (add a HUD element showing the text)
        var infoQuery = "writeln(show_hud(" + t.toString() + ")).";
        //var infoQuery = "_T = 'timepoint_" + t.toString() + "', " + "marker(hud_text('HUD'), TimeHudOld), marker_remove(TimeHudOld)," +
        //    "marker(hud_text('HUD'), TimeHudNew), marker_text(TimeHudNew, '" +  timeString + "'), marker_publish.";
        var prolog = that.client.newProlog();
        function processInfo(result) {
            setTimeout(function(){ handler(); }, 500);
        }
        prolog.jsonQuery(infoQuery, processInfo);
    };
    
    this.newVideo = function() {
        isNewVideo = true;
        that.replayEpisodeSelected();

        document.getElementById('replay-video-title-text').value = '';

        ace.edit('init_query').setValue('');
        ace.edit('user_query').setValue('');

        var firstrange = document.getElementById('replay-start-time');
        var secondrange = document.getElementById('replay-end-time');
        var select = document.getElementById('replay-episode-dropdown');

        firstrange.min = 0;
        firstrange.max = 0;
        firstrange.value = 0;

        secondrange.min = 0;
        secondrange.max = 0;
        secondrange.value = 0;

        select.value = 0;
        select.options[0].selected = true;
        that.setTimeSlides();
    };
};
