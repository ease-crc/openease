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
oe:result_set_show(QueryID,ResultSet) :-
	result_set_objects(ResultSet,Objs),
	%% TODO move member to aggregate query
	member(Obj,Objs),
	data_vis_affordance_graph(Obj,GraphData),
	GraphData \= [],
	% publish the message,
	data_vis_graph(QueryID, GraphData,
	    [title: 'Graph of typical affordances']).

%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_has_description(ResultSet),
	result_set_descriptions(ResultSet, Descrs),
	% %% TODO move member to aggregate query
	member(Desc,Descrs),
	data_vis_plan_graph(Desc,GraphData),
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
        kb_call(during([
            triple(Evt,dul:hasParticipant,Obj),
            ignore(triple(Role,dul:classifies,Obj)),
            ignore(once(triple(Tsk,dul:isTaskOf,Role))),
            once(triple(Evt,dul:isClassifiedBy,Tsk))
        ],Evt)),
        (   ( Edge=[Evt,Obj,1,2,'hasParticipant'] )
        ;   ( ground(Role),Edge=[Obj,Role,2,3,'hasRole'] )
        ;   ( ground(Tsk),Edge=[Role,Tsk,3,4,'hasTask'] )
        )
    ), EdgeData),
    % get DataVis graph data
    data_vis_rdf_graph(Evt,EdgeData,GraphData).

%%
data_vis_affordance_graph(Obj,GraphData) :-
    % get all edges
    findall(Edge, (
        kb_call((
            triple(Obj,soma:hasDisposition,D),
            ignore(triple(D,dul:isDescribedBy,A)),
            ignore(once((
                triple(A,dul:definesTask,Tsk),
                has_type(Tsk,TskType),
                subclass_of(TskType, dul:'Task')
            ))),
            ignore(once((
                triple(A,soma:definesTrigger,Trigger),
                has_type(Trigger, TriggerType),
                subclass_of(TriggerType, dul:'Role')
            ))),
            ignore(once((
                triple(A,soma:definesBearer,Bearer),
                has_type(Bearer, BearerType),
                subclass_of(BearerType, dul:'Role')
            )))
        )),
        (   ( Edge=[Obj,D,1,2,'hasDisposition'] )
        ;   ( ground(A),Edge=[D,A,2,3,'isDescribedBy'] )
        ;   ( ground(A),ground(TskType),Edge=[A,TskType,3,4,'definesTask'] )
        ;   ( ground(A),ground(TriggerType),Edge=[A,TriggerType,3,4,'definesTrigger'] )
        ;   ( ground(A),ground(BearerType),Edge=[A,BearerType,3,4,'definesBearer'] )
        )
    ), EdgeData),
    % get DataVis graph data
    data_vis_rdf_graph(Obj,EdgeData,GraphData).
%%
data_vis_plan_graph(Plan,GraphData) :-
    % get all edges
    findall(Edge, (
        kb_call((
            triple(Plan, soma:hasGoal, Goal),
            triple(Plan, dul:definesTask, Task),
            ignore(triple(Goal,dul:usesConcept,Role)),
            ignore(triple(Goal, dul:hasPart, ImageSchema))
        )),
        (   ( Edge=[Plan,Goal,1,2,'hasGoal'] )
        ;   ( ground(Task),Edge=[Plan,Task,1,2,'definesTask'] )
        ;   ( ground(Role),Edge=[Goal,Role,2,3,'usesConcept'] )
        ;   ( ground(ImageSchema),Edge=[Goal,ImageSchema,2,3,'hasPart'] )
        )
    ), EdgeData),
    % get DataVis graph data
    data_vis_rdf_graph(Plan,EdgeData,GraphData).
    
%%
data_vis_graph(ID,NodeData,Options) :-
    data_vis(graph(ID), [
    	array_data: NodeData | Options
    ]).

