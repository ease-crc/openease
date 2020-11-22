:- module(openease_vis,
    [ show(r),
      hide(r)
    ]).

use_module(library('ros/urdf')).

clear_canvas :-
	% TODO: implement me, should remove all marker
	true.

show(_) :-
	tripledb_load('package://knowrob/owl/robots/PR2.owl'),
	(	url_resolve('package://knowrob/urdf/pr2_for_unit_tests.urdf',Resolved)
	->	true
	;	Resolved='package://knowrob/urdf/pr2_for_unit_tests.urdf' 
	),
	urdf_load_file('http://knowrob.org/kb/PR2.owl#PR2_0', Resolved), 
	urdf_set_pose_to_origin('http://knowrob.org/kb/PR2.owl#PR2_0',map),
	sleep(1.0), 
	marker_plugin:republish.

hide(_) :-
	true.

%:- module(knowrob_vis,
%    [
%      show/1,
%      show/2,
%      hide/1
%    ]).
%/** <module> Methods for visualizing parts of the knowledge base
%
%  @author Daniel Beßler
%  @license BSD
%*/
%:- use_module(library('semweb/rdfs')).
%:- use_module(library('semweb/rdf_db')).
%:- use_module(library('knowrob/data_vis')).
%
%:- rdf_meta 
%      hide(t),
%      show(t),
%      show(t,t),
%      hide_marker(t),
%      show_marker(t,t).
%
%:- multifile show/2,
%             hide/1,
%             show_marker/2,
%             hide_marker/1.
%
%%% show(+Thing) is det.
%%% show(+Thing, +Properties) is det.
%%
%% This is a non-logical predicate used to invoke
%% external clients such us RViz
%% to visualize objects in the knowledge base.
%% Custom visualization back-ends may be used
%% by defining another clause of this multifile
%% predicate.
%%
%show(Things) :-
%  is_list(Things), !,
%  show_next_,
%  forall( member(Thing, Things), (
%    T =.. [show|Thing], call(T)
%  )), !.
%show(Thing) :-
%  show(Thing,[]).
%show(Thing, Properties) :-
%  show_marker(Thing,Properties).
%
%%% show_next is det
%show_next_ :-
%  true.
%
%%% hide(+Thing) is det.
%%
%% This is a non-logical predicate used to invoke
%% external clients such us RViz
%% to remove visualizations of objects.
%%
%hide(Things) :-
%  is_list(Things), !,
%  forall( member(Thing, Things), (
%    T =.. [hide|Thing], call(T)
%  )), !.
%hide(Thing) :-
%  hide_marker(Thing).
%  
%% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%% % % % % Marker visualization
%
%%%
%show_marker(Thing, Properties) :-
%  atom(Thing),
%  rdf_resource(Thing),
%  object_state(Thing, State, Properties),
%  object_state_add_cpp([State]).
%%%
%hide_marker(Thing) :-
%  atom(Thing),
%  rdf_resource(Thing),
%  object_state_remove_cpp([Thing]).
