
function waitForProlog(ros,then) {
    const pl = new ROSPrologClient(ros, {});
    if(!pl) return;
    pl.jsonQuery("true", function(result) {
        pl.finishClient();
        if(result.error) {
            setTimeout(function() { waitForProlog(ros, then) }, 500);
        }
        else {
            then();
        }
    });
};
