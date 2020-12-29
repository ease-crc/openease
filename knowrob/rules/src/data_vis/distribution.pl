:- module(distribution_handler,[]).

:- use_module(library(openease)).

%%
% TODO:
%%
oe:result_set_show(QueryID,ResultSet) :-
	result_set_events(ResultSet,[Evt]),
	% collect all tsks performed in phases
	task_distribution(Evt,Data),
	%% send message
	data_vis(barchart(QueryID),
			[ title: 'Distribution of executed tasks',
			  array_data: Data
			]
	).

%%
% TODO: idea: pie chart about how many actions failed vs. succeeded
%             pie chart of which tasks have failed most often
%%
%oe:result_set_show(QueryID,ResultSet) :-
%	result_set_events(ResultSet,[Evt]),
%	% collect all tsks performed in phases
%	task_distribution(Evt,Data),
%	%% send message
%	data_vis(piechart(QueryID),
%			[ title: 'Distribution of executed tasks',
%			  array_data: Data
%			]
%	).

%%
task_distribution(Evt, Data) :-
	% collect all tsks performed in phases
	findall([TskName,TskIRI,task],
		(	ask(aggregate([
		        transitive( reflexive(
		            triple(Evt,dul:hasConstituent,SubEvt)
		        )),
		        once(triple(SubEvt,dul:executesTask,Tsk))
		    ])),
		    rdf_db:rdf_split_url(X,TskName0,Tsk),
			atomic_list_concat([TskName,_],'_',TskName0),
		    rdf_db:rdf_split_url(X,TskName,TskIRI)
		),
		Tsks
	),
	Tsks \= [],
	list_to_set(Tsks,Tsks0),
	% count tsks
	findall([Tsk_a,[Count_a]],
		(	member(Tsk_a,Tsks0),
			include(=(Tsk_a),Tsks,Tsks_a),
			length(Tsks_a, Count_a)
		),
		Data
	).

