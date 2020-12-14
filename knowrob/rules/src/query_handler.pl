:- module(query_handler,
    [ 
    	openease_query(t,r)
    ]).

%:- use_module(library(data_vis)).

:- multifile openease_gen_answer/2.

/** <module> Tunnels queries of openEASE and executes other actions if necessary.

@author Daniel Beßler
@license BSD
*/

%% openease_query(+Query, +Mode) is nondet.
%
% True for statements that hold during the whole
% duration of some time interval.
%
% @param Query The query to be executed
% @param Mode A list of modes for the execution.
%
openease_query(_Query,Bindings) :-
	% TODO: handle non-atomic bindings
	%%
	% collect all events in Bindings
	findall(EvtAtom,
		(	member([_,Evt],Bindings),
			atom_string(EvtAtom,Evt),
			is_event(EvtAtom)
		),
		Events),
	openease_query_(event,Events),
	%%
	% collect all object in Bindings
	findall(ObjAtom,
		(	member([_,Obj],Bindings),
			atom_string(ObjAtom,Obj),
			is_object(ObjAtom)
		),
		Objects),
	openease_query_(object,Objects),
	%%
	% collect all values in Bindings
	findall(ValAtom,
		(	member([_,Val],Bindings),
			atom_string(ValAtom,Val)
		),
		Values),
	openease_query_(all,Values).

%%
openease_query_(_,[]) :- !.
openease_query_(EntityType,Entities) :-
	forall(
		openease_gen_answer(EntityType,Entities),
		true
	).
