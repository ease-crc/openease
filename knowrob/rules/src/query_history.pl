:- module(query_history,
		[ history_add/1,
		  history_get/2
		]).
		
%%
history_file('/home/ros/user_data/history.txt').

%%
%
history_add(Query) :-
	history_file(Path),
	open(Path,append,Stream),
	write(Stream,Query), nl(Stream), nl(Stream),
	close(Stream).

%%
%
history_get(Index,Query) :-
	history_read(Queries),
	length(Queries,Count),
	Index1 is Count-Index,
	nth1(Index1,Queries,Query).

%%
%
history_read(Queries) :-
	history_file(Path),
	exists_file(Path),
	open(Path,read,Stream),
	read_queries(Stream,Queries),
	close(Stream).

%%
%
history_write(Queries) :-
	history_file(Path),
	open(Path,write,Stream),
	forall(
		member(Q,Queries),
		( write(Stream,Q), nl(Stream), nl(Stream) )
	),
	close(Stream).

%%
trim(L,N,S) :-
  length(P,N),
  append(P,S,L).

%%
read_queries(S,[Q1|Qs]) :-
	read_line(S,L1),!,
	read_query(S,Ls),
	atomic_list_concat([L1|Ls], '\n',Q1),
	read_queries(S,Qs).
read_queries(_,[]).

%%
read_query(S,Lines) :-
	read_line(S,Line),
	(	Line=''
	->	Lines=[]
	;	(	read_query(S,Lines0),
			Lines=[Line|Lines0]
		)
	).

%%
read_line(S, X) :- 
	read_line_to_codes(S, L),
	L \= end_of_file,
	atom_codes(X, L).

%%
history_rotate(Max) :-
	history_read(Queries),
	length(Queries,Count),
	(	Count<Max ; (
		Diff is Count-Max,
		trim(Queries,Diff,Trimmed),
		history_write(Trimmed)
	)),
	!.

:- ignore(history_rotate(100)).

