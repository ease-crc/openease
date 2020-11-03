/**
 * A widget used to display query results.
 * Results can be successively added by calling addQueryResponse().
 */
function Blackboard(parent, qid, query_string) {
    var that = this;
    
    // the card div
    this.blackboard_div = undefined;
    this.blackboard_container = undefined;
    // number of received answers
    this.answer_count = 0;
    // 
    this.query_id = qid + "_query";
    
    this.create = function() {
        //
        var card = $("<div>");
        card.addClass("card border-info");
        //
        that.blackboard_div = $("<div>");
        that.blackboard_div.addClass("collapse show");
        that.blackboard_div.attr("id",that.query_id);
        card.append(that.blackboard_div);
        // create a container for card
        var query_col = $("<div>");
        query_col.addClass("col");
        query_col.append(card);
        that.blackboard_container = $("<div>");
        that.blackboard_container.addClass("row pb-2");
        that.blackboard_container.append(query_col);
        // Add container to parent
        $(parent).append(that.blackboard_container);
        // add input widget
        that.push("Input", that.createInputWidget());
        that.setupInputWidget();
    };

    this.delete = function() {
        that.blackboard_container.remove();
    };

    this.push = function(section,widget) {
        // TODO: allow pushing multiple widgets to same section?
        that.blackboard_div.append(that.createHeader(section));
        that.blackboard_div.append(widget);
    };

    this.createHeader = function(text) {
        var header = $("<div>");
        header.addClass("card-header ease-dark v");
        header.text(text);
        // TODO: clickable header
        //header.attr("data-toggle", "collapse");
        //header.attr("aria-expanded", "true");
        //header.attr("aria-controls", that.query_id);
        //header.attr("href", "#"+that.query_id);
        return header;
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
    
    this.finish = function () {
        // NOTE: this function is called when a query has no more answers
    };
    
    this.collapse = function () {
        //this.blackboard_div.addClass("collapse");
        //this.blackboard_div.collapse('hide');
    };

    /*********************************************/
    /*************** BINDINGS ********************/
    /*********************************************/

    /**
     * Add next Prolog solution to the widget.
     */
    this.addQueryResponse = function(response) {
        if(response.status == "QUERY_FAILED") {
            that.push("Failures", that.createExceptionWidget(response.error));
        }
        else if(response.status == "NO_SOLUTION") {
            if(that.answer_count==0) {
                that.push("Bindings", that.createFailedWidget());
            }
            that.finish();
            return;
        }
        else {
            that.push("Bindings", that.createSolutionWidget(response.solution));
        }
        that.answer_count += 1;
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
        var table = $("<table>");
        table.addClass("card-table table table-sm table-hover table-striped");
        table.append(tbody);
        return table;
    };

    // create a section in the query card displaying one solution
    this.createSolutionWidget = function(solution) {
        var body = $("<div>");
        body.attr("id", qid + that.answer_count);
        body.addClass("card-body border-top");
        //
        var item_container = $("<div>");
        item_container.addClass("container-fluid");
        //
        if(solution) {
            body.append(that.createBindingTable(solution));
        }
        else {
            var succeeded_item = $("<p>");
            succeeded_item.addClass("card-text");
            succeeded_item.text("Yes.");
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
        item.text("No.");
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
    this.addChart = function(section,data_vis_msg) {
        var chart = $("<div>");
        var chart_item = that.createItem(chart);
        chart_item.data_vis_msg = data_vis_msg;
        chart_item.data_vis_div = chart;
        var item = that.createItem(chart_item);
        that.push(section, item);
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

    /*********************************************/
    /************ INPUT WIDGET *******************/
    /*********************************************/

    this.createInputWidget = function () {
        //
        var query_text = $("<div>");
        query_text.attr("id",qid);
        query_text.addClass("card-text bg-transparent w-100 query-font");
        //
        var body = $("<div>");
        body.addClass("card-body");
        body.append(query_text);
        return body;
    };

    this.setupInputWidget = function () {
        // Use ace editor for stylized display
        // of query string.
        var ace_edit = ace.edit(qid);
        ace_edit.setTheme("ace/theme/solarized_light");
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
        ace_edit.setValue(query_string);
        ace_edit.clearSelection();
    };

    /*********************************************/
    /*********************************************/
    that.create();
};
