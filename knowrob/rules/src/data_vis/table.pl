:- module(table_handler,
	[ data_vis_table/3,
	  data_vis_rdf_table(r,t)
	]).

:- use_module(library(query_handler)).

query_handler:openease_gen_answer(all,Values) :-
	data_vis_rdf_table(Values, [title: 'Result Description']).
%%
data_vis_rdf_table(Values,Options) :-
	findall(
		[Type, Comment],
		(
			member(Val,Values),
			instance_of(Val, Type),
			has_comment(Type,Comment)
		),
		CommentData0),
	sort(CommentData0, CommentData),
	writeln(CommentData),
	% generate ID for the chart
	CommentData = [[FirstType,_]|_],
	writeln(FirstType),
	rdf_db:rdf_split_url(_,FirstTypeName,FirstType),
	writeln(FirstTypeName),
	atomic_list_concat(
		['table',FirstTypeName],'_',ID),
	writeln(ID),
	% publish the message
	data_vis_table(ID, CommentData, Options).

%%
% Publishes a data vis message with tree data.
%
data_vis_table(ID, TableData, Options) :-
	% need to map to DataVis message format here
	table_data_(0,TableData,ArrayData),
	writeln(ArrayData),
    data_vis(table(ID),
    	[array_data: ArrayData | Options]
    ).

table_data_(_,[],[[],[]]) :- !.
table_data_(Index,
		[[Type,Comment]|RestIn],
		[[Index,Index|RestIndices],[Type,Comment|RestContent]]
) :-
	NewIndex is Index + 1,
	table_data_(NewIndex,RestIn,[RestIndices,RestContent]).