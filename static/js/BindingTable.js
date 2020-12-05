/**
 * A table showing variable bindings inferred by Prolog.
 *
 * @param options
 * @constructor
 */
function BindingTable(options){
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

    this.create_table = function(solution) {
        let tbody = $("<tbody>");
        for (let key in solution) {
            let td1 = $("<td>");
            td1.text(key);
            //
            let td2 = $("<td>");
            td2.text(formatter.format(solution[key]));
            //
            let tr = $("<tr>");
            tr.append(td1);
            tr.append(td2);
            tbody.append(tr);
        }
        //
        let table = $("<table>");
        table.addClass("card-table table table-sm table-hover table-striped table-bordered");
        table.append(tbody);
        return table;
    };
}
