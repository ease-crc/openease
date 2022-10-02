/**
 * A table showing descriptions for the  bindings inferred by Prolog.
 *
 * @param options
 * @constructor
 */
function DescriptionTable(options){
    let that = this;
    options = options || {};

    const formatter = options.formatter || function(x){ return x; };

    this.widget = $("<div>");
    this.widget.addClass("card-body border-top");

    // create a section in the query card displaying one solution
    this.create = function(solution) {
        let item_container = $("<div>");
        item_container.addClass("container-fluid");
        //
        that.widget.text('');
        that.widget.append(that.create_table(solution));
        that.widget.append(item_container);
        return that.widget;
    };

    // Create a table of the solution description
    this.create_table = function(descriptions) {
        const tbody = $("<tbody>");

        for(let i = 0; i < descriptions[1].value1.length; i+=3) {
            const tr = $("<tr>");

            for(let j=0; j<3; j++) {
                const value = descriptions[1].value1[i+j];
                const td = $("<td>");
                const div = $("<div>");
                div.addClass("card-table-text");
                div.text(formatter.format(value));
                td.append(div);
                tr.append(td);
            }

            tbody.append(tr);
        }

        //
        var table = $("<table>");
        table.addClass("card-table table table-sm table-hover table-striped table-bordered");
        table.append(tbody);
        return table;
    };
}
