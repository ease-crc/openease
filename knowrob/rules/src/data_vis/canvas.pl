:- module(canvas_handler,[]).

:- use_module(library(query_handler)).

query_handler:openease_gen_answer(event,[Evt]) :-
	once(ask(aggregate([
		triple(Evt,dul:hasTimeInterval,Interval),
		triple(Interval,soma:hasIntervalBegin,Time0),
		triple(Interval,soma:hasIntervalEnd,Time1)
	]))),
	tf_plugin:tf_republish_set_goal(Time0,Time1),
	data_vis(type(evt,989),
		[values:[[Evt],[Time0,Time1]]]).

%data_vis_tf(StartTime, EndTime, Options) :-
%	% gather data
%	findall([[Frame,Time],[RefFrame,X,Y,Z,QX,QY,QZ,QW]],
%		tf_plugin:tf_mng_range(StartTime,EndTime,
%			Frame,
%			[RefFrame,[X,Y,Z],[QX,QY,QZ,QW]],Time,_),
%		ArrayData),
%	% generate ID for the chart
%	atomic_list_concat(
%		['tf',StartTime,EndTime],'_',ID),
%	% publish the message
%    data_vis(tf(ID),
%    	[array_data: ArrayData | Options]).

%query_handler:openease_gen_answer(event,Evts) :-
%	% TODO: handle multiple events
%	once(member(Evt,Evts)),
%%	ask(aggregate([
%%		triple(Evt,dul:hasTimeInterval,Interval),
%%		triple(Interval,soma:hasIntervalBegin,Time)
%%	])),
%	get_time(Time),
%    marker_plugin:show_markers(Time).
