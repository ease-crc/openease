:- module(distribution_handler,[]).

:- use_module(library(query_handler)).

query_handler:openease_gen_answer(event,_) :-
	atomic_list_concat(
		['piechart','testing'],'_',ID),
    data_vis(piechart(ID), [
        title: 'Some Distribution',
        data: [[a,b,c],[10,30,22]]
    ]).

query_handler:openease_gen_answer(event,_) :-
	atomic_list_concat(
		['barchart','testing'],'_',ID),
    data_vis(barchart(ID), [
        title: 'Some Distribution',
        data: [[a,b,c],[10,30,22]]
    ]).
