
function PlaybackWidget(canvas,options){
    options = options || {};
    var event_iri = options.event;
    var time_min = options.time_min;
    var time_max = options.time_max;
    var is_playing = true;
    var prolog = new ROSPrologClient(options.ros, {});

    var widget = $("<div>");
    widget.attr("id", "playback-progress-container");
    widget.append(canvas);

    var controls = $("<div>");
    controls.addClass("row m-auto btn-toolbar justify-content-center");
    widget.append(controls);

    var play_button = $("<a>");
    var play_icon = $("<div>");
    play_button.addClass("btn");
    play_icon.addClass("fas fa-pause");
    play_icon.attr("id", "playback-play");
    play_button.append(play_icon);
    play_button.click(function() {
        if(is_playing) {
            $("#playback-play")
                .removeClass("fa-pause")
                .addClass("fa-play");
            prolog.jsonQuery("tf_republish_set_realtime_factor(0).", function(result) {
                console.info("Paused playback");
                prolog.finishClient();
                $('#query-icon').removeClass('fa-spinner fa-spin').addClass('fa-question');
            });
        }
        else {
            $("#playback-play")
                .removeClass("fa-play")
                .addClass("fa-pause");
            prolog.jsonQuery("tf_republish_set_realtime_factor(1).", function(result) {
                console.info("Paused playback");
                prolog.finishClient();
                $('#query-icon').removeClass('fa-spinner fa-spin').addClass('fa-question');
            });
        }
        is_playing = !is_playing;
    });

    var button_group1 = $("<div>");
    button_group1.addClass("btn-group");
    button_group1.attr("id", "playback-btn-group");
    button_group1.append(play_button);
    controls.append(button_group1);

    var progress_bar = $("<div>");
    var progress_bar0 = $("<div>");
    var progress_text = $("<span>");
    progress_bar.addClass("progress m-auto");
    progress_bar0.addClass("progress-bar bg-info no-transition");
    progress_bar0.attr("id", "playback-progress");
    progress_bar0.append(progress_text);
    progress_bar.append(progress_bar0);

    var button_group2 = $("<div>");
    button_group2.addClass("btn-group");
    button_group2.append(progress_bar);
    controls.append(button_group2);

    this.getWidget = function() {
        return widget;
    }

    this.tick = function(time) {
        if(time<time_min) {
            val=0;
        }
        else if(time>time_max) {
            val=100;
        }
        else {
            val=Math.trunc(100.0*(time - time_min)/(time_max-time_min));
        }
        $("#playback-progress").css("width", val + "%");
        progress_text.text(new Date(time*1000.0).toLocaleTimeString());
    }
};
