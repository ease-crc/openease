:- module(hierachy_handler,
	[ data_vis_tree/3,
	  data_vis_rdf_tree(r,r,t),
	  data_vis_graph/3,
	  data_vis_rdf_graph(r,r,t)
	]).

:- use_module(library(query_handler)).

%%
%query_handler:openease_gen_answer(event,[Evt]) :-
%	data_vis_rdf_tree(Evt,
%		dul:hasConstituent,
%		[title: 'Phases']).

%query_handler:openease_gen_answer(event,[Evt]) :-
%	data_vis_rdf_tree(Evt,
%		dul:hasParticipant,
%		[title: 'Participation']).

query_handler:openease_gen_answer(object,[Obj]) :-
	data_vis_rdf_tree(Obj,
		dul:hasComponent,
		[title: 'Component hierarchy']).

%%
data_vis_rdf_tree(Root,Property,Options) :-
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
    data_vis(treechart(ID),
    	[array_data: ArrayData | Options]
    ).

tree_data_(Index-Index,[],[]) :- !.
tree_data_(Index-Index_out,
		[Node,Children],
		[[Node,Indices]|Rest]
) :-
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

%%
data_vis_rdf_graph_b(InitialNode,EdgeData,GraphData) :-
    %% format DataVis msg
    % 1. find set of all IRIs in the graph
    findall(IRI0,
        (   IRI0=InitialNode
        ;   member([_,IRI0|_],EdgeData)
        ), NodeIRIs0),
    list_to_set(NodeIRIs0,NodeIRIs),
    % 2. create nodes [[name,iri,group],[i0,...]]
    findall(Node,
        (   member(IRI1,NodeIRIs),
            data_vis_rdf_node(IRI1,NodeIRIs,EdgeData,Node)
        ),  GraphData).

%%
data_vis_rdf_node(IRI,NodeIRIs,EdgeData,
        [[Name,IRI,Group],Edges]) :-
	rdf_db:rdf_split_url(_,Name,IRI),
	% find node group in edge data
	once((
	    member([IRI,_,Group,_,_],EdgeData);
	    member([_,IRI,_,Group,_],EdgeData)
	)),
	% find edges
	findall(Edge,
	    (   member([IRI,Next_IRI,_,_,Relation],EdgeData),
	        nth0(NodeIndex,NodeIRIs,Next_IRI),
	        atomic_list_concat([Relation,NodeIndex],'_',Edge)
	    ),
	    Edges).

%%
data_vis_participant_graph(Evt,GraphData) :-
    % get all edges
    findall(Edge, (
        ask(during(aggregate([
            triple(Evt,dul:hasParticipant,Obj),
            ignore(triple(Role,dul:classifies,Obj)),
            ignore(once(triple(Tsk,dul:isTaskOf,Role)))
        ]),Evt)),
        (   ( Edge=[Evt,Obj,1,2,'hasParticipant'] )
        ;   ( ground(Role),Edge=[Obj,Role,2,3,'hasRole'] )
        ;   ( ground(Tsk),Edge=[Role,Tsk,3,4,'hasTask'] )
        )
    ), EdgeData),
    % get DataVis graph data
    data_vis_rdf_graph_b(Evt,EdgeData,GraphData).

%%
query_handler:openease_gen_answer(event,[Evt]) :-
	data_vis_participant_graph(Evt,GraphData),
	GraphData \= [],
	% generate ID for twe chart
	rdf_db:rdf_split_url(_,EvtName,Evt),
	atomic_list_concat(
		['graph','participation',EvtName],'_',ID),
	% publish the message,
	data_vis_graph(ID, GraphData,
	    [title: 'Participation']).

%query_handler:openease_gen_answer(event,[Evt]) :-
	%data_vis_rdf_graph(Evt,
	%	dul:hasConstituent,
	%	[title: 'Phases (graph)']).

%%
data_vis_rdf_graph(Root,Property,Options) :-
	rdf_tree_data_(Root,Property,[Node,Children]),
	Children \= [],
	% generate ID for twe chart
	Node = [RootName|_],
	rdf_db:rdf_split_url(_,PropertyName,Property),
	atomic_list_concat(
		['graph',RootName,PropertyName],'_',ID),
	% publish the message
	tree_data_(0-_,[Node,Children],ArrayData),
	data_vis_graph(ID, ArrayData, Options).

%%
data_vis_graph(ID,NodeData,Options) :-
    data_vis(graph(ID), [array_data: NodeData | Options]).