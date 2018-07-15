
global.jQuery = require("jquery"); // "jquery": "2.2.4"
global.$ = jQuery;
require('jquery-ui-bundle');
require('jquery-ui-bundle/jquery-ui.css');
require('layout');

/////////////////////////
$.fn.DataTable = require('datatables.net')( window, $ );
require('datatables.net-buttons');
// require('datatables/media/css/jquery.dataTables.css');

/////////////////////////
// bootstrap
global.bootstrap = require('bootstrap');
require('bootstrap/dist/css/bootstrap.css');

/////////////////////////
// overlay loading bar
global.iosOverlay = require('ios-overlay');
global.Spinner = require('spin');
global.createSpinner = function(node) {
    var parent = node || document.body;
    var opts = {
                lines: 13, // The number of lines to draw
                length: 11, // The length of each line
                width: 5, // The line thickness
                radius: 17, // The radius of the inner circle
                corners: 1, // Corner roundness (0..1)
                rotate: 0, // The rotation offset
                color: '#FFF', // #rgb or #rrggbb
                speed: 1, // Rounds per second
                trail: 60, // Afterglow percentage
                shadow: false, // Whether to render a shadow
                hwaccel: false, // Whether to use hardware acceleration
                className: 'spinner', // The CSS class to assign to the spinner
                zIndex: 2e9, // The z-index (defaults to 2000000000)
                top: 'auto', // Top position relative to parent in px
                left: 'auto' // Left position relative to parent in px
    };
    var target = document.createElement("div");
    parent.appendChild(target);
    return new Spinner(opts).spin(target);
};

/////////////////////////
// OpenGL
global.THREE = require('three');
global.dat = require('dat.gui');

/////////////////////////
// RobotWebTools
global.ROS3D  = require('ros3d');
global.ROSLIB = require('roslib');
global.EventEmitter2 = require('eventemitter2');

/////////////////////////
// Code editor
global.ace = require('brace');
require('brace/mode/prolog');
require('brace/mode/xml');
require('brace/theme/monokai');
require('brace/theme/solarized_light');
require('brace/ext/language_tools');
global.aceLangTools = ace.acequire("ace/ext/language_tools");

/////////////////////////
// Diagram visualization
global.d3 = require('d3');
global.d3tip = require('d3-tip')(d3);

/////////////////////////
// diffing things
global.jsondiffpatch = require('jsondiffpatch');
require('jsondiffpatch/public/formatters-styles/annotated.css');
require('jsondiffpatch/public/formatters-styles/html.css');

