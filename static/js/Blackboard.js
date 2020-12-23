/**
 * A widget used to display query results.
 * Results can be successively added by calling addQueryResponse().
 */
function Blackboard(options) {
    const that = this;

    const parent = options.where;
    //const qid = options.qid;
    const formatter = options.formatter;

    this.sections = {};
    this.charts = [];
    this.counter = 0;

    this.tick = function(time) {
        for(const i in that.charts) {
            const chart = that.charts[i];
            if(chart.chart_obj && chart.chart_obj.tick) {
                chart.chart_obj.tick(time);
            }
        }
    };

    this.delete = function() {
        parent.html('');
    };

    this.getSection = function(section) {
        if(!(section in this.sections)) {
            const widget_id = "blackboard-div-"+that.counter;
            that.counter += 1;

            const header = $("<div>");
            header.addClass("card-header ease-dark d-block");
            header.attr("data-toggle", "collapse");
            header.attr("aria-expanded", "true");
            header.attr("aria-controls", widget_id);
            header.attr("href", "#"+widget_id);
            header.append(section);
            const chevron = $("<i>");
            chevron.addClass("fa fa-chevron-down blackboard-chevron");
            header.append(chevron);
            parent.append(header);

            const widget = $("<div>");
            widget.addClass("collapse show blackboard-section");
            widget.attr("id", widget_id);
            parent.append(widget);

            this.sections[section] = widget;
        }
        return this.sections[section];
    }

    this.push = function(section,widget) {
        this.getSection(section).append(widget);
    };

    this.createItem = function(content_div,options) {
        const card_text = $("<div>");
        card_text.addClass("card-text");
        if(options===undefined || options.border) {
            card_text.addClass("ease-border");
        }
        card_text.append(content_div);
        //
        const card = $("<div>");
        card.addClass("card-body bg-light text-center");
        card.append(card_text);
        return card;
    };
    
    this.finish = function () {
        // NOTE: this function is called when a query has no more answers
    };

    this.select = function(section,entity,type) {
        console.info(['select',section,entity,type]);
    };

    this.selectEvent = function(section,entity) {
        that.select(section,entity,'event');
    };

    this.selectObject = function(section,entity) {
        that.select(section,entity,'object');
    };

    /*********************************************/
    /*************** BINDINGS ********************/
    /*********************************************/

    /**
     * Add next Prolog solution to the widget.
     */
    this.addQueryResponse = function(console,response) {
        let widget = new ResponseWidget({
            formatter: formatter
        });
        that.push(widget.title, widget.create(response));
        // TODO: answer count
        // TODO: need to call that.finish();?
        // that.finish();
    };

    /*********************************************/
    /********** Result Description ***************/
    /*********************************************/

    /**
     * Add next Prolog solution to the widget.
     */
    this.addResultDescription = function(console,id,response) {
        let widget = new DescriptionWidget({
            formatter: formatter
        });
        let div = widget.create(response);
        if(!widget.has_failure) {
            that.push(widget.title, div);
        }
    };

    /*********************************************/
    /****************** CHARTS *******************/
    /*********************************************/

    // create chart div
    this.addChart = function(data_vis_msg) {
        const chart = $("<div>");
        const chart_item = that.createItem(chart);
        chart_item.data_vis_msg = data_vis_msg;
        chart_item.data_vis_msg.width = parent.width();
        //chart_item.data_vis_msg.height = 100;
        chart_item.data_vis_div = chart;
        that.push(data_vis_msg.title, chart_item);
        chart_item.data_vis = new DataVis(
            chart_item.data_vis_div,
            chart_item.data_vis_msg,
            that);
        that.charts.push(chart_item.data_vis);
    };

    /*********************************************/
    /*************** MEDIA FILES *****************/
    /*********************************************/

    // create image div
    this.addImage = function(section,image_url) {
        const img = $("<img>");
        img.addClass("card-img-top");
        img.attr("src", image_url);
        //
        that.push(section, that.createItem(img));
    };

    // create video div
    this.addVideo = function(section,video_url) {
        const src = $("<source>");
        src.attr("src", video_url);
        //
        const vid = $("<video controls autoplay loop>");
        vid.addClass("card-img-top");
        vid.append(src);
        //
        that.push(section, that.createItem(vid));
    };
}
