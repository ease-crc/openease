:- module(query_history,
		[ history_add/1,
		  history_get/2
		]).

%%
% TODO: do not read file in every call. rather store
%       history in Prolog for faster query qithout IO
%%
		
%%
history_file('/home/ros/user_data/history.txt').

%%
write_entry_(Stream,Query) :-
	write(Stream,Query), nl(Stream),
	write(Stream,'---'), nl(Stream).

%%
%
history_add(Query) :-
	(	history_get(0,Query)
	->	true
	;	(	history_file(Path),
			open(Path,append,Stream),
			write_entry_(Stream,Query),
			close(Stream)
	)).

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
	close(Stream),
	!.

%%
%
history_write(Queries) :-
	history_file(Path),
	open(Path,write,Stream),
	forall(
		member(Q,Queries),
		write_entry_(Stream,Q)
	),
	close(Stream).

%%
trim(L,N,S) :-
  length(P,N),
  append(P,S,L).

%%
read_queries(S,[Q1|Qs]) :-
	read_query(S,Lines),
	atomic_list_concat(Lines,'\n',Q1),
	read_queries(S,Qs).
read_queries(_,[]).

%%
read_query(S,Lines) :-
	read_line(S,Line),
	read_query0(S,Line,Lines).

read_query0(_,'---',[]) :- !.
read_query0(S,X,[X|Xs]) :- read_query(S,Xs).

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

