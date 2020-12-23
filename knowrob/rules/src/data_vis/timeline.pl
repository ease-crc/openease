:- module(oe_timeline_vis,[]).

:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).
:- use_module(library(openease)).

%%
% Generate timeline data visualization messages.
%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_has_event(ResultSet),
	result_set_events(ResultSet,Evts),
	openease_timeline(QueryID,Evts).
	
%% openease_timeline(+Evts) is semidet.
%
openease_timeline(QueryID,[Ev0,Ev1|Evs]) :-
	%% Multiple events are part of answer set.
	findall([E,Task,Start,End],
		timeline_data_b([Ev0,Ev1|Evs],E,Task,Start,End),
		EventData),
	openease_timeline_data(EventData,
		[ id: QueryID,
		  title: 'Timeline of events'
		]).

openease_timeline(QueryID, [Evt]) :-
	%% A single event is part of answer set.
	setof([SubEvt,Task,Start,End],
		timeline_data_a(Evt,SubEvt,Task,Start,End),
		EventData),
	EventData=[_,_|_],
	openease_timeline_data(EventData,
		[ id: QueryID,
		  title: 'Timeline of activity phases'
		]).

openease_timeline_data(EventsData,Options) :-
    findall(X, (
        member([Evt,Tsk,_,_],EventsData),
        once((rdf_split_url(_,TskName,Tsk))),
        (X=Evt ; X=TskName)
    ), EvtNames),
    findall(Time, (
        member([_,_,Start,End], EventsData),
        atomic_list_concat([Start, End],'_',Time)
    ), EventExtends),
    data_vis(timeline(event_timeline),
        [values:[EvtNames,EventExtends] | Options]).

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

