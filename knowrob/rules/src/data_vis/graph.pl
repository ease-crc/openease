:- module(graph_handler,
	[ data_vis_graph/3,
	  data_vis_rdf_graph(r,r,t)
	]).

:- use_module(library(query_handler)).

%%
%query_handler:openease_gen_answer(event,[Evt]) :-
%	data_vis_rdf_graph(Evt,
%		dul:hasConstituent,
%		[title: 'Phases (graph)']).

%%
data_vis_rdf_graph(Root,Property,Options) :-
	tree_handler:rdf_tree_data_(Root,Property,[Node,Children]),
	Children \= [],
	% generate ID for thwe chart
	Node = [RootName|_],
	rdf_db:rdf_split_url(_,PropertyName,Property),
	atomic_list_concat(
		['graph',RootName,PropertyName],'_',ID),
	% publish the message
	tree_handler:tree_data_(0-_,[Node,Children],ArrayData),
	data_vis_graph(ID, ArrayData, Options).

%%
data_vis_graph(ID,NodeData,Options) :-
    data_vis(graph(ID), [array_data: NodeData | Options]).
