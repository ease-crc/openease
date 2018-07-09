
EASE.HighlightingPass = function(scene, camera, highlighter) {
    this.scene = scene;
    this.camera = camera;
    this.highlighter = highlighter;

    this.enabled = true;
    this.clear = true;
    this.needsSwap = false;
    this.renderToScreen = true;
};

EASE.HighlightingPass.prototype.setSize = function(width,height) {};
EASE.HighlightingPass.prototype.render =
    function (renderer, writeBuffer, readBuffer, delta) {
  // TODO: pull request for ros3djs to include readBuffer parameter in highlighter.renderHighlights
  var rendererProxy = {
    'render': function(scene,camera) {
        renderer.render(scene,camera,readBuffer)
    }
  };
  this.highlighter.renderHighlights(this.scene, rendererProxy, this.camera);
};
