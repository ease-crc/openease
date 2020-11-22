:- module(marker_vis_video,
	[ openease_video_start/1,
	  openease_video_stop/0
	]).

%%
video_directory('/home/ros/user_data/video').

%% openease_video_start is det.
%
openease_video_start :-
	video_directory(Dir),
	empty_directory_(Dir).

%% openease_video_stop is det.
%
openease_video_stop(FPS) :-
	video_directory(Dir),
	video_create(Dir,FPS).

%%
video_create(Dir,FPS) :-
	% first run mencoder to create video from JPG images
	atom_number(FPS_atom,FPS),
	atom_concat('type=jpg:fps=',FPS_atom,Arg_mf),
	exec('mencoder'(
		'mf://*.jpg',
		'-mf', Arg_mf,
		'-o', 'video.mpg',
		'-speed', 1,
		'-ofps', FPS_atom,
		'-ovc', 'lavc',
		'-lavcopts', 'vcodec=mpeg4:vbitrate=2500',
		'-oac', 'copy',
		'-of', 'mpeg'
	)),
	% second run avconv to get mp4 video
	exec('avconv'(
		'-i', 'video.mpg',
		'-vf', 'scale="trunc(iw/2)*2:trunc(ih/2)*2"',
		'-c:v', 'libx264',
		'video.mp4'
	)).

%%
empty_directory_(Dir) :-
	exists_directory(Dir),
	!,
	directory_files(Dir,Entries),
	forall(
		(	member(Entry,Entries),
			atomic_list_concat([Dir,Entry],'/',File),
			is_file(File)
		),
		delete_file(File)
	).

empty_directory_(Dir) :-
	% \+ exists_directory(Dir),
	make_directory(Dir).

