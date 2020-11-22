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
