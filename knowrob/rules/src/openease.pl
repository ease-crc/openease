:- module(oe,
			[ openease_query(+,t,r),
			  result_set/2,
			  result_set_has_entity/1,
			  result_set_has_event/1,
			  result_set_has_object/1,
			  result_set_has_quality/1,
			  result_set_has_description/1,
			  result_set_has_situation/1,
			  result_set_entities/2,
			  result_set_events/2,
			  result_set_objects/2,
			  result_set_qualities/2,
			  result_set_descriptions/2,
			  result_set_situations/2
			]).

:- multifile result_set_show/2.

%% openease_query(+QueryID, +Query, +Mode) is nondet.
%
% True for statements that hold during the whole
% duration of some time interval.
%
% @param Query The query to be executed
% @param Mode A list of modes for the execution.
%
openease_query(QueryID,_Query,Bindings) :-
	% build result set
	result_set(Bindings,ResultSet),
	% generate data visualizations
	forall(
		result_set_show(QueryID,ResultSet),
		true
	).

%% create_result_set(+Bindings,-ResultSet) is det.
%
%
result_set(Bindings,
		[ events(Evts),
		  objects(Objs),
		  qualities(Quals),
		  descriptions(Descrs),
		  situations(Sits)
		]) :-
	% FIXME: rather use aggregate query to find entity+types
	result_set_entities_(Bindings,is_event,Evts),
	result_set_entities_(Bindings,is_physical_object,Objs),
	result_set_entities_(Bindings,is_quality,Quals),
	result_set_entities_(Bindings,is_description,Descrs),
	result_set_entities_(Bindings,is_situation,Sits).

%%
result_set_entities_([],_,[]) :- !.

result_set_entities_([[_,X]|Xs],Goal,[IRI|Ys]) :-
	string(X),
	atom_string(IRI,X),
	call(Goal, IRI),
	!,
	result_set_entities_(Xs,Goal,Ys).

result_set_entities_([_|Xs],Goal,Ys) :-
	result_set_entities_(Xs,Goal,Ys).

%% result_set_has_entitiy(+RS) is semidet.
%
% True iff an event is part of the result set.
%
result_set_has_entity(RS) :- result_set_has_event(RS),!.
result_set_has_entity(RS) :- result_set_has_object(RS),!.
result_set_has_entity(RS) :- result_set_has_quality(RS),!.
result_set_has_entity(RS) :- result_set_has_description(RS),!.
result_set_has_entity(RS) :- result_set_has_situation(RS),!.

%% result_set_has_event(+RS) is semidet.
%
% True iff an event is part of the result set.
%
result_set_has_event(RS) :-
	\+ result_set_events(RS,[]).

%% result_set_has_object(+RS) is semidet.
%
% True iff an object is part of the result set.
%
result_set_has_object(RS) :-
	\+ result_set_objects(RS,[]).

%% result_set_has_quality(+RS) is semidet.
%
% True iff a quality is part of the result set.
%
result_set_has_quality(RS) :-
	\+ result_set_qualities(RS,[]).

%% result_set_has_description(+RS) is semidet.
%
% True iff a description is part of the result set.
%
result_set_has_description(RS) :-
	\+ result_set_descriptions(RS,[]).

%% result_set_has_situation(+RS) is semidet.
%
% True iff a situation is part of the result set.
%
result_set_has_situation(RS) :-
	\+ result_set_situations(RS,[]).

%% result_events(+ResultSet,-Evts) is det.
%
% Get set of events that are part of the result set.
%
result_set_events(ResultSet,Evts) :-
	member(events(Evts),ResultSet),!.

%% result_set_objects(+ResultSet,-Objs) is det.
%
% Get set of objects that are part of the result set.
%
result_set_objects(ResultSet,Objs) :-
	member(objects(Objs),ResultSet),!.

%% result_set_qualities(+ResultSet,-Qs) is det.
%
% Get set of qualities that are part of the result set.
%
result_set_qualities(ResultSet,Qs) :-
	member(qualities(Qs),ResultSet),!.

%% result_set_descriptions(+ResultSet,-Descrs) is det.
%
% Get set of descriptions that are part of the result set.
%
result_set_descriptions(ResultSet,Descrs) :-
	member(descriptions(Descrs),ResultSet),!.

%% result_set_situations(+ResultSet,-Sits) is det.
%
% Get set of situations that are part of the result set.
%
result_set_situations(ResultSet,Sits) :-
	member(situations(Sits),ResultSet),!.

%% result_set_entities(+ResultSet,-X) is det.
%
% Get set of entities that are part of the result set.
%
result_set_entities(ResultSet,X) :-
	result_set_situations(ResultSet,X0),
	result_set_descriptions(ResultSet,X1),
	result_set_events(ResultSet,X2),
	result_set_objects(ResultSet,X3),
	result_set_qualities(ResultSet,X4),
	result_set_entities_([X0,X1,X2,X3,X4],X).

result_set_entities_([],[]) :- !.
result_set_entities_([X0|Xs],X) :-
	result_set_entities_(Xs,X1),
	append(X0,X1,X).

