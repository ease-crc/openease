
global.jQuery = require("jquery");
global.$ = jQuery;
require('jquery-ui-bundle');
require('jquery-ui-bundle/jquery-ui.css');
require('layout');

/////////////////////////
$.fn.DataTable = require('datatables.net')( window, $ );
require('datatables.net-buttons');
// require('datatables/media/css/jquery.dataTables.css');

/////////////////////////
// bootstrap & fontawesome
global.bootstrap = require('bootstrap');
require('bootstrap/dist/css/bootstrap.css');
global.fontawesome = require('@fortawesome/fontawesome-free');
require('@fortawesome/fontawesome-free/js/all.js');
require('typeface-oswald');

/////////////////////////
// OpenGL
global.THREE = require('three');
global.dat = require('dat.gui');

/////////////////////////
// RobotWebTools
global.ROS3D  = require('ros3d');
global.ROSLIB = require('roslib');
global.EventEmitter2 = require('eventemitter2');

var ros_clients = require('@openease/ros-clients');
global.ROSClient         = ros_clients.ROSClient;
global.ROSPrologClient   = ros_clients.ROSPrologClient;
global.MarkerArrayClient = ros_clients.MarkerArrayClient;

/////////////////////////
// QA console
global.PrologConsole = require('@openease/rosprolog-console');

/////////////////////////
// Code editor
global.ace = require('brace');
require('brace/mode/prolog');
require('brace/mode/xml');
require('brace/theme/monokai');
require('brace/ext/language_tools');
global.aceLangTools = ace.acequire("ace/ext/language_tools");

/////////////////////////
// Diagram visualization
global.d3 = require('d3');
global.d3tip = require('d3-tip')(d3);
global.DataVis = require('@openease/charts');

/////////////////////////
// Canvas visualization
global.EASEViewer = require('@openease/canvas-three');
