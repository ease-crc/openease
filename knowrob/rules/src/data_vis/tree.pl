:- module(oe_tree_vis,
	[ data_vis_tree/3,
	  data_vis_rdf_tree(r,r,t)
	]).

:- use_module(library(openease)).

%%
% Generate tree data visualization messages.
%%
oe:result_set_show(ResultSet) :-
	result_set_has_object(ResultSet),
	result_set_objects(ResultSet,Objs),
	member(Obj,Objs),
	data_vis_rdf_tree(Obj,
		dul:hasComponent,
		[title: 'Component hierarchy']).

%%
data_vis_rdf_tree(Root,Property,Options) :-
	%%%
	%% TODO: use transitive query here, problem is that at the moment
	%%       there is no way to obtain proper parent.
	%%%
	rdf_tree_data_(Root,Property,[Node,Children]),
	Children \= [],
	% generate ID for the chart
	Node = [RootName|_],
	rdf_db:rdf_split_url(_,PropertyName,Property),
	atomic_list_concat(
		['tree',RootName,PropertyName],'_',ID),
	% publish the message
	data_vis_tree(ID, [Node,Children], Options).
	
rdf_tree_data_(IRI,Property,[[Name,IRI,Group],ChildrenData]) :-
	% event name is displayed without IRI prefix
	rdf_db:rdf_split_url(_,Name,IRI),
	% nodes can be assigned to different groups
	% TODO: add actions/states/processes/motions to different groups
	Group is 0,
	% get event constituent data
	findall(ChildData,
		(	triple(IRI,Property,Child),
			rdf_tree_data_(Child,Property,ChildData)
		),
		ChildrenData
	).

%%
% Publishes a data vis message with tree data.
%
data_vis_tree(ID, TreeData, Options) :-
	% need to map to DataVis message format here
	tree_data_(0-_,TreeData,ArrayData),
	data_vis(treechart(ID), [
		array_data: ArrayData | Options
	]).

tree_data_(Index-Index,[],[]) :- !.
tree_data_(Index-Index_out,
		[Node,Children],
		[[Node,Indices]|Rest]) :-
	length(Children,NumChildren),
	findall(ChildIndex,
		(	between(1,NumChildren,N),
			ChildIndex is Index + N
		),
		Indices
	),
	%%
	Index0 is Index + NumChildren,
	tree_data_1_(Index0-Index_out,Children,Rest).

%%
tree_data_1_(Index-Index,[],[]) :- !.
tree_data_1_(Index_0-Index_n,[X|Xs],ArrayData) :-
	tree_data_(Index_0-Index_1,X,Y),
	tree_data_1_(Index_1-Index_n,Xs,Ys),
	append(Y,Ys,ArrayData).

