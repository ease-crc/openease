Syntax
-----------------------------------------------------------------

There are only three basic constructs in Prolog:  facts, rules, and queries.
A collection of facts and rules is called a knowledge base (or a database) and
Prolog programming is all about writing knowledge bases.
That is, Prolog programs simply are knowledge bases, collections of
facts and rules which describe some collection of relationships that we find interesting.

So how do we use a Prolog program? By posing queries.
That is, by asking questions about the information stored in the knowledge base.

Now this probably sounds rather strange.
It's certainly not obvious that it has much to do with programming at all.
After all, isn't programming all about telling a computer what to do?
But as we shall see, the Prolog way of programming makes a lot of sense,
at least for certain tasks; for example, it is useful in computational linguistics
and AI. But instead of saying more about Prolog in general terms,
let's jump right in and start writing some simple knowledge bases;
this is not just the best way of learning Prolog, it's the only way.

Unification
-----------------------------------------------------------------

When working with the knowledge bases in the previous sections,
we briefly mentioned the idea of unification.
We said, for example, that Prolog unifies `robot(X)` with `robot(pr2)`,
thereby instantiating the variable `X` to `pr2`.
It's now time to take a closer look at unification,
for it is one of the most fundamental ideas in Prolog.

Recall that there are three types of term:

  * *Constants*. These can either be atoms (such as pr2) or numbers (such as 42).
  * *Variables*. (Such as X, and Z3)
  * *Complex terms*. These have the form: functor(term_1,...,term_n).

We are going to work with a basic intuition, which is a little light on detail:

*Two terms unify if they are the same term or if they contain variables
that can be uniformly instantiated with terms in such a way that
the resulting terms are equal.*

This means, for example,
that the terms `pr2` and `pr2` unify, because they are the same atom.
Similarly, the terms `42` and `42` unify, because they are the same number,
the terms `X` and `X` unify, because they are the same variable,
and the terms `robot(pr2)` and `robot(pr2)` unify, because they
are the same complex term. The terms `robot(pr2)` and `robot(boxy)`,
however, do not unify, as they are not the same (and neither of them contains
a variable that could be instantiated to make them the same).

Now, what about the terms `pr2` and `X`? They are not the same.
However, the variable `X` can be instantiated to `pr2` which makes them equal.
So, by the second part of our working definition, `pr2` and `X` unify.
Similarly, the terms `robot(X)` and `robot(pr2)` unify,
because they can be made equal by instantiating `X` to `pr2`.
How about `before(ev3,X)` and `before(X,ev2)`? No.
It is impossible to find an instantiation of `X` that makes the two terms equal.
Do you see why? Instantiating `X` to `ev2` would give us the terms `before(ev3,ev2)`
and `before(ev2,ev2)`, which are obviously not equal.

Usually we are not only interested in the fact that two terms unify,
we also want to know how the variables have to be instantiated to make them equal.
And Prolog gives us this information.
When Prolog unifies two terms it performs all the necessary instantiations,
so that the terms really are equal afterwards. This functionality,
together with the fact that we are allowed to build complex terms
(that is, recursively structured terms) makes unification a powerful
programming mechanism.

The `=/2` predicate tests whether its two arguments unify.
This is usually written in infix notation.
For example, if we pose the query
    
    pr2 = pr2.

Prolog will respond *yes*. Note that `=(pr2,pr2)` equivalent to the infix notation.
Now let's look at an example involving complex terms:

    k(s(g),Y)  =  k(X,t(k)).

Clearly the two complex terms unify if the stated variable instantiations
are carried out. But how does this follow from the definition?
The first thing we need to do is check that both complex terms have the
same functor and arity. And they do.
Then, we have to unify the corresponding arguments in each complex term.
So do the first arguments, `s(g)` and `X`, unify?
Yes, and we instantiate `X` to `s(g)`. So do the second arguments,
`Y` and `t(k)`, unify? Again yes, and we instantiate `Y` to `t(k)`.

<h>`Task 1`: Test which of the following terms unify. Can you explain why?</h>

`a)`

    'Pr2' = pr2.

`b)`

    'pr2' = pr2.

`c)`

    Pr2 = pr2.


<h>`Task 2`: Here are six Italian words: *astante*, *astoria*,
*baratto*, *cobalto*, *pistola*, *statale*.
They are to be arranged, crossword puzzle fashion, in the following grid: </h>

<div align="center">
<img src="http://www.learnprolognow.org/html/crosswd2.eps.png" alt="Smiley face" width="320">
</div>
<br>

Write a predicate `crossword/6` that tells us how to fill in the grid.

<div class="tut-editor">
% declare words as facts in the
% Prolog knowledge base.
word([a,s,t,a,n,t,e]).
word([a,s,t,o,r,i,a]).
word([b,a,r,a,t,t,o]).
word([c,o,b,a,l,t,o]).
word([p,i,s,t,o,l,a]).
word([s,t,a,t,a,l,e]).

crossword(V1,V2,V3,H1,H2,H3) :-
	%%
	% Write your code here...
	%%
	%%
	% Make sure that every word has been used.
	%%
	forall(word(X), member(X,[V1,V2,V3,H1,H2,H3])).
</div>

The first three arguments should be the vertical words from left to right,
and the last three arguments the horizontal words from top to bottom:

    crossword(V1,V2,V3,H1,H2,H3).

Facts
-----------------------------------------------------------------

Knowledge Base 1 (KB1.pl) is simply a collection of facts.
Facts are used to state things that are unconditionally true of some situation
of interest. For example, we can state that `pr2`, `boxy`, and `pepper` are robots,
and that pepper can talk, using the following facts:

<div class="tut-editor">
robot(pr2). 
robot(boxy). 
robot(pepper). 
can_talk(pepper). 
</div>

This collection of facts is *KB1*. It is our first example of a Prolog program.
Note that the names `pr2`, `boxy`, and `pepper`, and the properties `robot` and
`can_talk` have been written so that the first letter is in lower-case.
This is important; we will see why a little later on.

How can we use *KB1*? By posing queries. That is, by asking questions about
the information *KB1* contains. Here is an example. We can ask Prolog whether
`pr2` is a `robot` by posing the query:

    robot(pr2).

Prolog will answer *yes* for the obvious reason that this is one of the
facts explicitly recorded in *KB1*.
The answer is *no* for a query such as `can_talk(pr2)`.
This is because the knowledge base does not encode information about that
`pr2` can talk. Note that Prolog does not distinguish between *don't know*
and *definitely not* and assumes any statement is false if no information is
available to proof it (*closed world assumption*).

Let's now make use of variables. Here's an example:

    robot(X).

The `X` is a variable (in fact, any word beginning with an upper-case letter
is a Prolog variable, which is why we had to be careful to use lower-case
initial letters). Now a variable isn't a name, rather it's a placeholder
for information. That is, this query asks Prolog: tell me which of the
individuals you know about is a robot.

Prolog answers this query by working its way through *KB1*, from top to bottom,
trying to unify (or match) the expression `robot(X)` with the information
*KB1* contains. Now the first item in the knowledge base is `robot(pr2)`.
So, Prolog unifies `X` with `pr2`, thus making the query agree
perfectly with this first item. (Incidentally, there's a lot of
different terminology for this process: we can also say that Prolog
instantiates `X` to `pr2`, or that it binds `X` to `pr2`.)
Prolog then reports back to us as follows:

<pre>
X = pr2
</pre>

That is, it not only says that there is information about at
least one robot in *KB1*, it actually tells us who that is.
It didn't just say yes, it actually gave us the variable binding (or variable
instantiation) that led to success. \n\nBut that's not the end of the story.
The whole point of variables is that they can stand for, or unify with,
different things. And there is information about other robots in the
knowledge base. We can access this information by pressing the query
button multiple times.

So Prolog begins working through the knowledge
base again (it remembers where it got up to last time and starts from there) and
sees that if it unifies `X` with `boxy`, that a fact `robot(boxy)`
is also stored in *KB1*, and so forth. So it responds:

<pre>
X = pr2 ;
X = boxy ;
X = pepper.
</pre>

The business of unifying variables with information in the knowledge base is
the heart of Prolog. As we'll learn, there are many interesting ideas in
Prolog -- but when you get right down to it, it's Prolog's ability to perform
unification and return the values of the variable bindings to us that is crucial.

Prolog also knows if-then-else statements,
but in a rather unusual notation using `->` to separate condition from consequences,
and using `;` to separate consequences.

    robot(R),
    ( can_talk(R) ->
      Speech='hello'
    ; Speech='...' ).

`Task 1`: Extend *KB1* with some facts about components of robots.
State that ...
  
  * ... arms and grippers are components
  * ... `pr2_left_arm`, `pr2_right_arm`, `boxy_arm` are components with type `arm`
  * ... `pr2_left_gripper`, `pr2_right_gripper`, `boxy_gripper` are components
     with type `gripper`
  * ... `pr2` has the components `pr2_left_arm`,
     `pr2_right_arm`, `pr2_left_gripper`, and `pr2_right_gripper`
  * ... `boxy` has the component `boxy_arm` and `boxy_gripper`

<div class="tut-editor">
%%
% Write your code here...
%%
</div>

Test your KB with following queries:

`a)`

    has_component(pr2,Component)

`b)`

    has_component(Robot,C), has_type(C,gripper)

Recursion
-----------------------------------------------------------------

As its name suggests, a *list* is just a plain old list of items.
Slightly more precisely, it is a finite sequence of elements.
Here are some examples of lists in Prolog:

<pre>
[pr2, boxy, pepper]

[pr2, robot(pr2), X, 2, pepper]

[]

[pr2, [boxy, pepper], [pr2, robot(pepper)]]

[[], can_talk(z), [2, [b, c]], [], Z, [2, [b, c]]]
</pre>

Here are some observations:

  * We can specify lists in Prolog by enclosing the elements of the list in square brackets
  * All sorts of Prolog objects can be elements of a list.
  * The empty list (as its name suggests) is the list that contains no elements.
  * Lists can contain other lists as elements.

Now for an important point. Any non-empty list can be thought of as
consisting of two parts: the head and the tail. The head is simply
the first item in the list; the tail is everything else. To put it more
precisely, the tail is the list that remains when we take the first element
away; that is, the tail of a list is always a list .
For example, the head of

<pre>
[pr2, boxy, pepper]
</pre>

is `pr2`, and the tail is `[boxy, pepper]`.
And what are the head and the tail of the list `[can_talk(z)]`?
Well, the head is the first element of the list, which is `can_talk(z)`,
and the tail is the list that remains if we take the head away, which,
in this case, is the empty list `[]`.

Prolog has a special built-in operator `|` which can be used to
decompose a list into its head and tail. It is important to get to know
how to use `|`, for it is a key tool for writing Prolog list
manipulation programs.

The most obvious use of `|` is to extract
information from lists. We do this by using `|` together with unification.
For example, to get hold of the head and tail of `[pr2,boxy,pepper]` we
can pose the following query:

    [Head|Tail] = [pr2, boxy, pepper].

That is, the head of the list has become bound to `Head` and the tail of
the list has become bound to `Tail`.

Let's look at some other examples.
We can extract the head and tail of the following list just as we saw above:

    [X|Y] = [[], can_talk(z), [2, [b, c]], [], Z].

That is: the head of the list is bound to `X`,
the tail is bound to `Y`. (We also learn that Prolog has bound `Z`
to the internal variable starting with `_`)

But we can do a lot more with
`|`, it really is a flexible tool. For example, suppose we wanted
to know what the first two elements of the list were, and also the remainder
of the list after the second element. Then we'd pose the following query:

    [X,Y|W] = [[], can_talk(z), [2, [b, c]], [], Z].

That is, the head of the list is bound to `X`, the second element is bound
to `Y`, and the remainder of the list after the second element is bound to
`W` (that is, `W` is the list that remains when we take away the first
two elements). So `|` can not only be used to split a list into its head and
its tail, we can also use it to split a list at any point.
To the left of `|` we simply indicate how many elements we want to take
away from the front of the list, and then to right of the | we will get what remains.

Prolog offers a set of built-in predicates that operate on lists.
`length/2` being one example
which yields the number of list elements in the second argument.
Another example is `sum_list/2` which sums up numerical elements
in a list.

It's time to look at our first example of a recursive Prolog program
for manipulating lists. One of the most basic things we would like
to know is whether something is an element of a list or not. So let's
write a program that, when given as inputs an arbitrary object `X` and a list `L`,
tells us whether `X` belongs to `L`.
The program that does this is usually called `member`,
and it is the simplest example of a Prolog program that exploits the
recursive structure of lists. Here it is:

<pre>
member(X,[X|T]).
member(X,[H|T]) :- member(X,T).
</pre>

That's all there is to it: one fact (namely `member(X,[X|T])` ) and one
rule (namely `member(X,[H|T]) :- member(X,T)` ).
But note that the rule is recursive (after all, the functor member occurs
in both the rule's head and body) and it is this that explains why such a
short program is all that is required.\n\nWe could summarise the `member/2`
predicate as follows. It is a recursive predicate, which systematically
searches down the length of the list for the required item.
It does this by stepwise breaking down the list into smaller lists,
and looking at the first item of each smaller list.
This mechanism that drives this search is recursion,
and the reason that this recursion is safe (that is, the reason it does
not go on forever) is that at the end of the line Prolog
has to ask a question about the empty list. The empty list cannot be
broken down into smaller parts, and this allows a way out of the recursion.

Another Prolog essential is the predicate `findall/3` whose second argument
is a goal for which different bindings are collected in the third argument
which is a list.
The first argument of `findall` defines the term to be recorded for each
successful instantiation of arguments in the goal.

    findall(R, robot(R), Robots).

This yields a list holding the names of robots in our KB.

<h>`Task 1`: Test which of the following terms unify. Can you explain why?</h>

`a)`

    [a,b,c,d] = [a,[b,c,d]].

`b)`

    [a,b,c,d] = [a|[b,c,d]].

`c)`

    [a,b,c,d] = [a,b,[c,d]].

`d)`

    [a,b,c,d] = [a,b|[c,d]].

`e)`

    [a,b,c,d] = [a,b,c,[d]].

`f)`

    [a,b,c,d] = [a,b,c|[d]].

`g)`

    [a,b,c,d] = [a,b,c,d,[]].

`h)`

    [a,b,c,d] = [a,b,c,d|[]].

`i)`

    [] = _.

`j)`

    [] = [_].

`k)`

    [] = [_|[]].

<h>`Task 2`: Write a predicate `second(X,List)` which checks
whether `X` is the second element of `List`.</h>

<div class="tut-editor">
second(X,List) :-
	%%
	% Write your code here...
	%%
	fail.
</div>

Test your declaration with following query:

    second([a,4|_],X).

<h>`Task 3`: Write a predicate `swap12(List1,List2)` which checks whether
`List1` is identical to `List2`, except that the first two elements are
exchanged.</h>

<div class="tut-editor">
swap12(List1,List2) :-
	%%
	% Write your code here...
	%%
	fail.
</div>

Test your declaration with following queries:

`a)`

    swap12([b,a,c,d],[a,b,c,d]).

`b)`

    swap12(X,[a,b|A]),
    X=[b,a|A].

<h>`Task 4`: Write a predicate `twice(In,Out)` whose left argument is a list, and whose right argument is a list consisting of every element in the left list written twice.</h>

<div class="tut-editor">
twice(In,Out) :-
	%%
	% Write your code here...
	%%
	fail.
</div>

Test your declaration with following query:

    twice([a,4,[pr2,pepper]],X),
    X = [a,a,4,4,[pr2,pepper],[pr2,pepper]].
