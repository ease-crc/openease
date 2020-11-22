:- module(timeline_handler,[]).

:- use_module(library(query_handler)).

query_handler:openease_gen_answer(event,[Ev0,Ev1|Evs]) :-
    findall([E,Task,Start,End],
    	(	member(E,[Ev0,Ev1|Evs]),
	    	timeline_data(E,Task,Start,End)
	    ),
    	EventData),
    data_vis:timeline_data(EventData).

query_handler:openease_gen_answer(event,[Evt]) :-
	setof([SubEvt,Task,Start,End],
		(	get_subevent(Evt,SubEvt),
			timeline_data(SubEvt,Task,Start,End)
		),
		EventData),
	EventData=[_,_|_],
	data_vis:timeline_data(EventData,
		[ title: 'Activity phases'
		]).

%%
get_subevent(Evt,Evt).
get_subevent(Evt,SubEvent) :-
	triple(Evt,dul:hasConstituent,X),
	get_subevent(X,SubEvent).

%%
timeline_data(E,Task,Start,End) :-
	ask(aggregate([
				triple(E,rdf:type,dul:'Event'),
				triple(E,rdf:type,regex('^.*(?!Action).*')),
				triple(E,dul:isClassifiedBy,TaskInstance),
				triple(E,dul:hasTimeInterval,Interval),
				triple(Interval,soma:hasIntervalBegin,Start),
				triple(Interval,soma:hasIntervalEnd,End)
			])),
	atomic_list_concat([Task,_],'_',TaskInstance).
