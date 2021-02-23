:- module(oe_canvas_vis,[]).

:- use_module(library(openease)).

%%
% Generate canvas visualization messages.
%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_has_event(ResultSet),
	result_set_events(ResultSet,Evts),
	openease_canvas(QueryID,Evts).

%%
% Generate canvas visualization messages.
%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_has_object(ResultSet),
	result_set_objects(ResultSet,Objs),
	maplist(has_base_link_name, Objs, Links),
	data_vis(type(obj,977), [
      id: QueryID,
      values: [ Links, [] ]
	]).

%% openease_canvas(+Evts) is semidet.
%
openease_canvas(QueryID,[Evt]) :-
	%%
	% TODO: handle situation where multiple events are shown
	% TODO: handler situation where no event but an object is part of result set
	%%
	once(ask(aggregate([
		triple(Evt,dul:hasTimeInterval,Interval),
		triple(Interval,soma:hasIntervalBegin,Time0),
		triple(Interval,soma:hasIntervalEnd,Time1)
	]))),
	tf_plugin:tf_republish_set_goal(Time0,Time1),
	data_vis(type(evt,989), [
	    id: QueryID,
		values: [
			[Evt],
			[Time0,Time1]
		]
	]).