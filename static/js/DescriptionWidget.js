/**
 * A widget showing a description of the bindings of a KnowRob query.
 *
 * @param options
 * @constructor
 */
function DescriptionWidget(options){
    const that = this;
    options = options || {};

    this.title = "Response description";
    this.has_failure = false;
    this.widget = undefined;
    this.answer_count = 0;

    this.create = function(response) {
        return that.create_solution_widget(response);
    };

    this.create_solution_widget = function(solution) {
        return new DescriptionTable(options).create(solution);
    };

}
