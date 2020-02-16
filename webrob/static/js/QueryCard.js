/**
 * A widget used to display query results.
 * Results can be succesively added by calling addQueryResponse().
 */
function QueryCard(container,qid,query_string) {
    var that = this;
    
    // the card div
    this.query_card = undefined;
    // last answer that was received
    this.last_answer = undefined;
    // number of received answers
    this.answer_count = 0;
    // 
    this.card_id = qid + "_card";
    
    // 
    this.charts = [];
    this.images = [];
    this.videos = [];
    
    this.create = function() {
        var header = that.createHeader("Query");
        header.addClass("text-white bg-info");
        // clickable header
        // TODO: better add a small button in the header that does it.
        header.attr("data-toggle", "collapse");
        header.attr("aria-expanded", "true");
        header.attr("aria-controls", that.card_id);
        header.attr("href", "#"+that.card_id);
        //
        var card = $("<div>");
        card.addClass("card border-info");
        card.append(header);
        that.query_card = $("<div>");
        that.query_card.attr("id",that.card_id);
        card.append(that.query_card);
        //
        that.query_card.append(that.createQueryDiv());
        // A bootstrap column
        var query_col = $("<div>");
        query_col.addClass("col");
        query_col.append(card);
        // Add column to row
        var query_row = $("<div>");
        query_row.addClass("row pb-2");
        query_row.append(query_col);
        // Add row to card container.
        $(container).append(query_row);
        // Use ace editor for stylized display
        // of query string.
        var ace_edit = ace.edit(qid);
        ace_edit.setTheme("ace/theme/solarized_light");
        ace_edit.getSession().setMode("ace/mode/prolog");
        ace_edit.setOptions({
            maxLines: 15,
            autoScrollEditorIntoView: true,
            readOnly: true,
            showGutter: false,
            printMarginColumn: false,
            highlightActiveLine: false,
            highlightGutterLine: false
        });
        ace_edit.setValue(query_string);
    };
    
    this.createHeader = function(text) {
        var header = $("<div>");
        header.addClass("card-header text-white");
        header.text(text);
        return header;
    };
    
    this.createQueryDiv = function() {
        var query_text = $("<div>");
        query_text.attr("id",qid);
        query_text.addClass("card-text history-query");
        //
        var body = $("<div>");
        body.addClass("card-body");
        body.append(query_text);
        return body;
    };
    
    /**
     * Add next Prolog solution to the widget.
     */
    this.addQueryResponse = function(response) {
        var body = $("<div>");
        body.attr("id", qid + that.answer_count);
        body.addClass("card-body border-top");
        //
        if(response.status == "NO_SOLUTION") {
            if(that.answer_count==0) {
                var item = $("<p>");
                item.addClass("card-text");
                item.text("No.");
                //
                var item_container = $("<div>");
                item_container.addClass("container-fluid");
                item_container.append(that.createItem(item));
                body.append(item_container);
                that.query_card.append(body);
            }
            that.finish();
            return;
        }
        that.query_card.append(body);
        if(response.status == "QUERY_FAILED") {
            that.createFailedWidget(body,response.error);
        }
        else {
            that.createSolutionWidget(body,response.solution);
        }
        that.last_answer = body;
        that.answer_count += 1;
    };
    
    this.createItem = function(content_div) {
        var card_text = $("<div>");
        card_text.addClass("card-text");
        card_text.append(content_div);
        //
        var card = $("<div>");
        card.addClass("card bg-light text-center");
        card.append(card_text);
        return card;
    };
    
    // create chart div
    this.addChart = function(data_vis_msg) {
        var chart = $("<div>");
        var chart_item = that.createItem(chart);
        chart_item.data_vis_msg = data_vis_msg;
        chart_item.data_vis_div = chart;
        that.charts.push(chart_item);
    };
    
    // create image div
    this.addImage = function(image_url) {
        var img = $("<img>");
        img.addClass("card-img-top");
        img.attr("src", image_url);
        //
        var item = that.createItem(img);
        that.images.push(item);
    };
    
    // create video div
    this.addVideo = function(video_url) {
        var src = $("<source>");
        src.attr("src", video_url);
        //
        var vid = $("<video controls autoplay loop>");
        vid.addClass("card-img-top");
        vid.append(src);
        //
        var item = that.createItem(vid);
        that.images.push(item);
    };
    
    // Create a 2-column table variable-value
    this.createBindingTable = function(solution) {
        var tbody = $("<tbody>");
        for (var key in solution) {
            var td1 = $("<td>");
            td1.text(key);
            //
            var td2 = $("<td>");
            td2.text(solution[key].toString());
            //
            var tr = $("<tr>");
            tr.append(td1);
            tr.append(td2);
            tbody.append(tr);
        }
        //
        var caption = $("<caption>");
        caption.text("Bindings");
        //
        var table = $("<table>");
        table.addClass("card-table table table-sm table-hover table-striped");
        table.append(caption);
        table.append(tbody);
        return table;
    };
    
    // create a section in the query card displaying one solution
    this.createSolutionWidget = function(card,solution) {
        var item_container = $("<div>");
        item_container.addClass("container-fluid");
        //
        if(solution) {
            var bindingTable = that.createBindingTable(solution);
            card.append(bindingTable);
        }
        else {
            var hasItems = (that.charts.length>0) ||
                           (that.images.length>0) ||
                           (that.videos.length>0);
            if(!hasItems) {
                var succeeded_item = $("<p>");
                succeeded_item.addClass("card-text");
                succeeded_item.text("Yes.");
                item_container.append(that.createItem(succeeded_item));
            }
        }
        //
        card.append(item_container);
        // render charts
        for(var chart_index in that.charts) {
            var chart = that.charts[chart_index];
            item_container.append(chart);
            chart.data_vis = new DataVis(
                chart.data_vis_div,
                chart.data_vis_msg);
        }
        // render images
        for(var image_index in that.images) {
            var image = that.images[image_index];
            item_container.append(image);
        }
        // render videos
        for(var video_index in that.videos) {
            var video = that.videos[video_index];
            item_container.append(video);
        }
        // reset massage items
        // FIXME: probably messages may be received delayed.
        //        better would be to have an explicit association to qid.
        that.charts = [];
        that.images = [];
        that.videos = [];
    };
    
    this.createFailedWidget = function(card,error) {
        var widget = $("<p>");
        widget.addClass("card-text alert alert-warning text-left");
        // get error message
        var msg = error.solution || error.message || error.toString();
        // TODO: pretty print prolog terms
        widget.text(msg.toString());
        //
        var item_container = $("<div>");
        item_container.addClass("container-fluid");
        item_container.append(that.createItem(widget));
        card.append(item_container);
    };
    
    this.finish = function () {};
    
    this.collapse = function () {
        // FIXME: collapses without animation.
        this.query_card.addClass("collapse");
    };
    
    that.create();
};
