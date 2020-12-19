:- module(oe_timeline_vis,[]).

:- use_module(library(openease)).

%%
% Generate timeline data visualization messages.
%%
oe:result_set_show(ResultSet) :-
	result_set_has_event(ResultSet),
	result_set_events(ResultSet,Evts),
	openease_timeline(Evts).
	
%% openease_timeline(+Evts) is semidet.
%
openease_timeline([Ev0,Ev1|Evs]) :-
	%% Multiple events are part of answer set.
	findall([E,Task,Start,End],
		timeline_data_b([Ev0,Ev1|Evs],E,Task,Start,End),
		EventData),
	data_vis:timeline_data(EventData,
		[ title: 'Timeline of events'
		]).

openease_timeline([Evt]) :-
	%% A single event is part of answer set.
	setof([SubEvt,Task,Start,End],
		timeline_data_a(Evt,SubEvt,Task,Start,End),
		EventData),
	EventData=[_,_|_],
	data_vis:timeline_data(EventData,
		[ title: 'Timeline of activity phases'
		]).

%%
timeline_data_a(Evt,Sub,Task,Start,End) :-
	ask(aggregate([
		transitive(reflexive(triple(Evt,dul:hasConstituent,Sub))),
		triple(Sub,rdf:type,dul:'Event'),
		triple(Sub,rdf:type,regex('^.*(?!Action).*')),
		triple(Sub,dul:isClassifiedBy,TaskInstance),
		triple(Sub,dul:hasTimeInterval,Interval),
		triple(Interval,soma:hasIntervalBegin,Start),
		triple(Interval,soma:hasIntervalEnd,End)
	])),
	atomic_list_concat([Task,_],'_',TaskInstance).

%%
timeline_data_b(Evts,E,Task,Start,End) :-
	ask(aggregate([
		triple(in(Evts) -> E,rdf:type,dul:'Event'),
		triple(E,rdf:type,regex('^.*(?!Action).*')),
		triple(E,dul:isClassifiedBy,TaskInstance),
		triple(E,dul:hasTimeInterval,Interval),
		triple(Interval,soma:hasIntervalBegin,Start),
		triple(Interval,soma:hasIntervalEnd,End)
	])),
	atomic_list_concat([Task,_],'_',TaskInstance).

