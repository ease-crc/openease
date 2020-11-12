/**
 * A widget used to display query results.
 * Results can be successively added by calling addQueryResponse().
 */
function Blackboard(parent, qid, query_string) {
    var that = this;

    // number of received answers
    this.answer_count = 0;
    // 
    this.query_id = qid + "_query";

    this.delete = function() {
        parent.html('');
    };

    this.push = function(section,widget) {
        // TODO: allow pushing multiple widgets to same section?
        parent.append(that.createHeader(section));
        parent.append(widget);
    };

    this.createHeader = function(text) {
        var header = $("<div>");
        header.addClass("card-header ease-dark");
        header.text(text);
        return header;
    };

    this.createItem = function(content_div) {
        var card_text = $("<div>");
        card_text.addClass("card-text");
        card_text.append(content_div);
        //
        var card = $("<div>");
        card.addClass("card-body bg-light text-center");
        card.append(card_text);
        return card;
    };
    
    this.finish = function () {
        // NOTE: this function is called when a query has no more answers
    };

    /*********************************************/
    /*************** BINDINGS ********************/
    /*********************************************/

    /**
     * Add next Prolog solution to the widget.
     */
    this.addQueryResponse = function(console,response) {
        if(response.status == "QUERY_FAILED") {
            that.push("Failure", that.createExceptionWidget(response.error));
        }
        else if(response.status == "NO_SOLUTION") {
            if(that.answer_count==0) {
                that.push("Result", that.createFailedWidget());
            }
            that.finish();
            return;
        }
        else {
            that.push("Result", that.createSolutionWidget(console,response.solution));
        }
        that.answer_count += 1;
    };

    // Create a 2-column table variable-value
    this.createBindingTable = function(console,solution) {
        var tbody = $("<tbody>");
        for (var key in solution) {
            var td1 = $("<td>");
            td1.text(key);
            //
            var td2 = $("<td>");
            td2.text(console.format(solution[key]));
            //
            var tr = $("<tr>");
            tr.append(td1);
            tr.append(td2);
            tbody.append(tr);
        }
        //
        var table = $("<table>");
        table.addClass("card-table table table-sm table-hover table-striped table-bordered");
        table.append(tbody);
        return table;
    };

    // create a section in the query card displaying one solution
    this.createSolutionWidget = function(console,solution) {
        var body = $("<div>");
        body.attr("id", qid + that.answer_count);
        body.addClass("card-body border-top");
        //
        var item_container = $("<div>");
        item_container.addClass("container-fluid");
        //
        if(solution) {
            body.append(that.createBindingTable(console,solution));
        }
        else {
            var succeeded_item = $("<p>");
            succeeded_item.addClass("card-text");
            succeeded_item.text("true");
            item_container.append(that.createItem(succeeded_item));
        }
        //
        body.append(item_container);
        return body;
    };

    //
    this.createFailedWidget = function() {
        var body = $("<div>");
        body.attr("id", qid + that.answer_count);
        body.addClass("card-body border-top");
        //
        var item = $("<p>");
        item.addClass("card-text");
        item.text("false");
        //
        var item_container = $("<div>");
        item_container.addClass("container-fluid");
        item_container.append(that.createItem(item));
        //
        body.append(item_container);
        return body;
    };

    /*********************************************/
    /****************** CHARTS *******************/
    /*********************************************/

    // create chart div
    this.addChart = function(data_vis_msg) {
        var chart = $("<div>");
        var chart_item = that.createItem(chart);
        chart_item.data_vis_msg = data_vis_msg;
        chart_item.data_vis_msg.width = parent.width();
        //chart_item.data_vis_msg.height = 100;
        chart_item.data_vis_div = chart;
        that.push(data_vis_msg.title, chart_item);
        chart_item.data_vis = new DataVis(
            chart_item.data_vis_div,
            chart_item.data_vis_msg);
    };

    /*********************************************/
    /*************** MEDIA FILES *****************/
    /*********************************************/

    // create image div
    this.addImage = function(section,image_url) {
        var img = $("<img>");
        img.addClass("card-img-top");
        img.attr("src", image_url);
        //
        that.push(section, that.createItem(img));
    };

    // create video div
    this.addVideo = function(section,video_url) {
        var src = $("<source>");
        src.attr("src", video_url);
        //
        var vid = $("<video controls autoplay loop>");
        vid.addClass("card-img-top");
        vid.append(src);
        //
        that.push(section, that.createItem(vid));
    };

    /*********************************************/
    /*************** FAILURES ********************/
    /*********************************************/

    this.createExceptionWidget = function(error) {
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
        return item_container;
    };
};
