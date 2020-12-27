:- module(oe_table_vis,
	[ data_vis_table/3
	]).

:- use_module(library(openease)).

%%
oe:result_set_show(QueryID,ResultSet) :-
	%%
	result_set_has_entity(ResultSet),
	result_set_entities(ResultSet,Entities),
	%%
	show_comment_table(QueryID,Entities).

%%
show_comment_table(QueryID,Entities) :-
	%%
	% TODO: use aggregate query instead, problem is comments are not stored
	%        in triples collection
	%%
	%%
	findall(
		[Val, Type, Comment],
		(
			member(Val,Entities),
			triple(Val, rdf:type, Type),
			has_comment(Type,Comment)
		),
		CommentData0),
	sort(CommentData0, CommentData),
	CommentData \= [],
	% publish the message
	data_vis_table(QueryID, CommentData,
		[title: 'Response description']).

%%
% Publishes a data vis message with tree data.
%
data_vis_table(ID, TableData, Options) :-
	% need to map to DataVis message format here
	table_data_(0,TableData,ArrayData),
    data_vis(table(ID),
    	[array_data: ArrayData | Options]
    ).

table_data_(_,[],[[],[]]) :- !.
table_data_(Index,
		[[IRI,Type,Comment]|RestIn],
		[[Index,Index|RestIndices],[IRI,Type,Comment|RestContent]]
) :-
	NewIndex is Index + 1,
	table_data_(NewIndex,RestIn,[RestIndices,RestContent]).

