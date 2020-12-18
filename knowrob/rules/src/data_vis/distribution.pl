:- module(distribution_handler,[]).

:- use_module(library(openease)).

%%
% TODO:
%%
oe:result_set_show(ResultSet) :-
	result_set_events(ResultSet,[Evt]),
	% collect all tsks performed in phases
	task_distribution(Evt,Tsks0,Counts),
	%% generate id
	atomic_list_concat(
		['piechart','phases'],'_',ID),
	%% send message
	data_vis(piechart(ID),
			[ title: 'Distribution of tasks executed in event phases',
			  data: [Tsks0,Counts]
			]
	).

oe:result_set_show(ResultSet) :-
	result_set_events(ResultSet,[Evt]),
	% collect all tsks performed in phases
	task_distribution(Evt,Tsks0,Counts),
	%% generate id
	atomic_list_concat(
		['barchart','phases'],'_',ID),
	%% send message
	data_vis(barchart(ID),
			[ title: 'Distribution of tasks executed in event phases',
			  data: [Tsks0,Counts]
			]
	).


%%
task_distribution(Evt,Tsks0,Counts) :-
	% collect all tsks performed in phases
	findall(TskName,
		(	(	SubEvt=Evt
			;	transitive(triple(Evt,dul:hasConstituent,SubEvt))
			),
			once((
				triple(SubEvt,dul:executesTask,Tsk),
				rdf_db:rdf_split_url(_,TskName0,Tsk),
				atomic_list_concat([TskName,_],'_',TskName0)
			))
		),
		Tsks
	),
	Tsks \= [],
	list_to_set(Tsks,Tsks0),
	% count tsks
	findall(Count,
		(	member(Tsk_a,Tsks0),
			include(=(Tsk_a),Tsks,Tsks_a),
			length(Tsks_a, Count)
		),
		Counts
	).

