:- module(distribution_handler,[]).

:- use_module(library(openease)).

%%
% TODO:
%%
oe:result_set_show(_ResultSet) :-
	atomic_list_concat(
		['piechart','testing'],'_',ID),
    data_vis(piechart(ID), [
        title: 'Some Distribution',
        data: [[a,b,c],[10,30,22]]
    ]).

oe:result_set_show(_ResultSet) :-
	atomic_list_concat(
		['barchart','testing'],'_',ID),
    data_vis(barchart(ID), [
        title: 'Some Distribution',
        data: [[a,b,c],[10,30,22]]
    ]).

