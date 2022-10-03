:- module(oe_trajectory_vis,
	[ ]).

:- use_module(library(openease)).

%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_has_event(ResultSet),
	result_set_has_objects(ResultSet),
	result_set_objects(ResultSet, Objs),
	result_set_events(ResultSet, Evts),
	show_trajectory(QueryID, Evts, Objs),

show_trajectory(QueryID,[Evt], [Obj]) :-
	%%
	% TODO: handle situation where multiple objects are in the answer set
	%%
	once(kb_call([
		triple(Evt,dul:hasTimeInterval,Interval),
		triple(Interval,soma:hasIntervalBegin,Time0),
		triple(Interval,soma:hasIntervalEnd,Time1)
	])),
	findall(
		Trajectory,
		trajectory_marker_publish(Obj, Time0, Time1, Trajectory),
		Trajectories),
	marker_plugin:show_trajectories(Trajectory).
