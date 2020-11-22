:- module(distribution_handler,[]).

:- use_module(library(query_handler)).

query_handler:openease_gen_answer(event,_) :-
	atomic_list_concat(
		['plot','testing'],'_',ID),
    data_vis(linechart(ID), [
        title: 'Some plot',
        data: [[a,b,c],[10,30,22]]
    ]).
