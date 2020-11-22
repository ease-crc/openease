:- module(marker_vis_camera,
	[ camera_pose/2 ]).

%% camera_pose(+Position:list, +Orientation:list) is det
%
% Sends a pose via the ROS topic _|/camera/pose|_.
% Visualization clients may choose to manipulate some 3D camera accordingly.
%
% @param Position [float x,y,z]
% @param Orientation [float qx,qy,qz,qw]
%
camera_pose([X,Y,Z], [QX,QY,QZ,QW]) :-
	ros_publish('/camera/pose', 'geometry_msgs/Pose', _{
		position:    _{x: X, y: Y, z: Z},
		orientation: _{x: QX, y: QY, z: QZ, w: QW}
	}).

