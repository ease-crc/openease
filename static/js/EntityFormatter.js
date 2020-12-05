function EntityFormatter() {
    let that = this;

    this.rdf_namespaces = {};

    this.connect = function(ros,then) {
        let pl = new ROSPrologClient(ros, {});
        if(!pl) return;
        pl.jsonQuery("findall([_X,_Y], rdf_current_ns(_X,_Y), NS).",
            function(result) {
                pl.finishClient();
                if(result.solution) {
                  let namespaces = {};
                  for(const i in result.solution.NS) {
                      namespaces[result.solution.NS[i][1]] = result.solution.NS[i][0];
                  }
                  that.rdf_namespaces = namespaces;
                }
                then();
            }
        );
    };

    this.format = function(val) {
        let value_formatted = val;
        for(let iri in that.rdf_namespaces) {
            if(value_formatted.indexOf(iri) !== -1) {
                value_formatted = value_formatted.replace(
                    new RegExp(iri, 'g'), that.rdf_namespaces[iri]+":'")+"'";
            }
        }
        return value_formatted;
    };
};
