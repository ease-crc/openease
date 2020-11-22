:- module(marker_vis_highlight,
	[ highlight/1,
	  highlight/2,
	  unhighlight/1
	]).

%% highlight(+Objects) is det.
%
%
highlight(Objects) :-
	highlight(Objects,[1,0,0,1]).

%% highlight(+Objects,+Color) is det.
%
%
highlight(Object,Color) :-
	atom(Object),!,
	highlight([Object],Color).

highlight(Objects,Color) :-
	is_list(Objects),!,
	get_msg_(Objects,Color,Msg),
	ros_publish('/openease/highlight', 'knowrob_openease/Highlight', Msg).

%% unhighlight(+Objects) is det.
%
%
unhighlight(Objects) :-
	highlight(Objects,[0,0,0,0]).

%%
get_msg_(Objects,[R,G,B],Msg) :-
	get_msg_(Objects,[R,G,B,1],Msg), !.
 
get_msg_(Objects,[R,G,B,A],_{
		objects: ['array(string)',Objects],
		color: ['std_msgs/ColorRGBA',_{r:[float32,R],
		                               g:[float32,G],
		                               b:[float32,B],
		                               a:[float32,A]}]
}).

