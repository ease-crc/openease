/**
 * @author Russell Toris - rctoris@wpi.edu
 * @author Nils Berg - berg.nils@gmail.com
 */

/**
 * A MarkerArray client that listens to a given topic.
 *
 * Emits the following events:
 *
 *  * 'change' - there was an update or change in the MarkerArray
 *
 * @constructor
 * @param options - object with following keys:
 *
 *   * ros - the ROSLIB.Ros connection handle
 *   * topic - the marker topic to listen to
 *   * tfClient - the TF client handle to use
 *   * canvas - ...
 *   * path (optional) - the base path to any meshes that will be loaded
 */
EASE.MarkerArrayClient = function(options) {
  options = options || {};
  this.ros = options.ros;
  this.topicName = options.topic;
  this.tfClient = options.tfClient;
  this.canvas = options.canvas;
  this.path = options.path || '/';

  // Markers that are displayed (Map ns+id--Marker)
  this.markers = {};
  this.rosTopic = undefined;

  this.subscribe();
};
EASE.MarkerArrayClient.prototype.__proto__ = EventEmitter2.prototype;

EASE.MarkerArrayClient.prototype.subscribe = function(){
  this.unsubscribe();

  // subscribe to MarkerArray topic
  this.rosTopic = new ROSLIB.Topic({
    ros : this.ros,
    name : this.topicName,
    messageType : 'visualization_msgs/MarkerArray',
    compression : 'png'
  });
  this.rosTopic.subscribe(this.processMessage.bind(this));
};

EASE.MarkerArrayClient.prototype.processMessage = function(arrayMessage){
  var that = this;
  
  arrayMessage.markers.forEach(function(message) {
    var markerName = message.ns + message.id;
    if(message.action === 0) {
      var updated = false;
      if(markerName in this.markers) { // "MODIFY"
        var [marker,node] = that.markers[markerName];
        updated = marker.update(message);
        if(!updated) { // "REMOVE"
          node.unsubscribeTf();
          that.canvas.removeMarker(marker,node);
          delete that.markers[markerName];
        }
      }
      if(!updated) { // "ADD"
        var newMarker = new ROS3D.Marker({
          message : message,
          path : this.path,
        });
        newMarker.isSelectable = true; // XXX
        newMarker.isSceneOrtho = false; // XXX
        newMarker.id = message.id;
        newMarker.ns = message.ns;
        newMarker.frame_id = message.header.frame_id; // XXX
        newMarker.marker_type = message.type; // XXX
        var newNode = new ROS3D.SceneNode({
          frameID : message.header.frame_id,
          tfClient : this.tfClient,
          object : newMarker
        });
        this.markers[markerName] = [newMarker,newNode];
        that.canvas.addMarker(newMarker,newNode);
      }
    }
    else if(message.action === 1) { // "DEPRECATED"
      console.warn('Received marker message with deprecated action identifier "1"');
    }
    else if(message.action === 2 && markerName in this.markers) { // "DELETE"
      var [marker,node] = that.markers[markerName];
      node.unsubscribeTf();
      canvas.removeMarker(marker,node);
      delete that.markers[markerName];
    }
    else if(message.action === 3) { // "DELETE ALL"
        for (var [marker,node] in that.markers){
          node.unsubscribeTf();
          canvas.removeMarker(marker,node);
        }
        that.markers = {};
    }
    else {
      console.warn('Received marker message with unknown action identifier "'+message.action+'"');
    }
  }.bind(this));

  this.emit('change');
};

EASE.MarkerArrayClient.prototype.unsubscribe = function(){
  if(this.rosTopic){
    this.rosTopic.unsubscribe();
  }
};
