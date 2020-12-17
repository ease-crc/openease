/**
 * A widget showing the response of a KnowRob query.
 *
 * @param options
 * @constructor
 */
function ResponseWidget(options){
    const that = this;
    options = options || {};

    this.title = "Response";
    this.has_failure = false;
    this.widget = undefined;
    this.answer_count = 0;

    this.create = function(response) {
        if(response.status === "QUERY_FAILED") {
            that.has_failure = true;
            that.widget = that.create_exception_widget(response.error);
        }
        else if(response.status === "NO_SOLUTION") {
            that.widget = that.create_failed_widget();
        }
        else {
            that.widget = that.create_solution_widget(response.solution);
        }
        return that.widget;
    };

    this.create_solution_widget = function(solution) {
        if(solution) {
            return new BindingTable(options).create(solution);
        }
        else {
            const succeeded_item = $("<p>");
            succeeded_item.addClass("card-text p-1 alert alert-info");
            succeeded_item.text("true");
            return succeeded_item;
        }
    };

    this.create_exception_widget = function(error) {
        const widget = $("<p>");
        widget.addClass("card-text alert alert-danger text-left");
        widget.css('white-space','pre-wrap');
        // get error message
        const msg = error.solution || error.message || error.toString();
        // TODO: pretty print prolog terms
        widget.text(msg.toString());
        //
        return widget;
    };

    this.create_failed_widget = function() {
        const item = $("<p>");
        item.addClass("card-text p-1 alert alert-warning");
        item.text("false");
        return item;
    };

    this.create_item = function(content_div) {
        const card_text = $("<div>");
        card_text.addClass("card-text");
        card_text.addClass("ease-border");
        card_text.append(content_div);
        //
        let card = $("<div>");
        card.addClass("card-body bg-light text-center");
        card.append(card_text);
        return card;
    };
}
