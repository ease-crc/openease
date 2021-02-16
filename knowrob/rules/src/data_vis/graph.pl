:- module(oe_graph_vis,
	[ data_vis_graph/3,
	  data_vis_rdf_graph(r,r,t)
	]).

:- use_module(library(openease)).

%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_has_event(ResultSet),
	result_set_events(ResultSet,Evts),
	%% TODO move member to aggregate query
	member(Evt,Evts),
	data_vis_participant_graph(Evt,GraphData),
	GraphData \= [],
	% publish the message,
	data_vis_graph(QueryID, GraphData,
	    [title: 'Graph of event participation']).

%%
data_vis_rdf_graph(InitialNode,EdgeData,GraphData) :-
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
        [[Name,IRI,Group,Type],Edges]) :-
	rdf_db:rdf_split_url(_,Name,IRI),
	% find node group in edge data
	once((
	    member([IRI,_,Group,_,_],EdgeData);
	    member([_,IRI,_,Group,_],EdgeData)
	)),
	node_type_(Group,Type),
	% find edges
	findall(Edge,
	    (   member([IRI,Next_IRI,_,_,Relation],EdgeData),
	        nth0(NodeIndex,NodeIRIs,Next_IRI),
	        atomic_list_concat([Relation,NodeIndex],'_',Edge)
	    ),
	    Edges).

%%
node_type_(1,event) :- !.
node_type_(2,object) :- !.
node_type_(3,role) :- !.
node_type_(4,event_type) :- !.

%%
data_vis_participant_graph(Evt,GraphData) :-
    % get all edges
    findall(Edge, (
        ask(during(aggregate([
            triple(Evt,dul:hasParticipant,Obj),
            ignore(triple(Role,dul:classifies,Obj)),
            ignore(once(triple(Tsk,dul:isTaskOf,Role))),
            once(triple(Evt,dul:isClassifiedBy,Tsk))
        ]),Evt)),
        (   ( Edge=[Evt,Obj,1,2,'hasParticipant'] )
        ;   ( ground(Role),Edge=[Obj,Role,2,3,'hasRole'] )
        ;   ( ground(Tsk),Edge=[Role,Tsk,3,4,'hasTask'] )
        )
    ), EdgeData),
    % get DataVis graph data
    data_vis_rdf_graph(Evt,EdgeData,GraphData).

%%
data_vis_graph(ID,NodeData,Options) :-
    data_vis(graph(ID), [
    	array_data: NodeData | Options
    ]).

