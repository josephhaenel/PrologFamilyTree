% family Tree in prolog, with basic facts and rules for parent, grandather, etc
% could be extended to include sister, cousin, uncle, descendant, etc

% fatherOf
father(don,ted).
father(don,barb).
father(don,paula).
father(greg,erin).
father(greg,austin).
father(wes,alyssa).
father(ted,jessica).
father(ted,david).

mother(audrey,ted).
mother(audrey,barb).
mother(audrey,paula).
mother(paula,erin).
mother(paula,austin).
mother(barb,alyssa).

married(don,audrey).
married(wes,barb).
married(greg,paula).

male(don).
male(ted).
male(wes).
male(greg).
male(austin).
male(david).

female(audrey).
female(barb).
female(paula).
female(alyssa).
female(jessica).
female(erin).

parent(X,Y) :-
	father(X,Y); mother(X,Y).


grandfather(X,Y) :-
	father(X,Z), parent(Z,Y).
grandfather(don, ethan).

samefather(X,Y) :- 
	father(F,X),father(F,Y).

samemother(X,Y) :- 
	mother(M,X),mother(M,Y).

sameparent(X,Y) :- samefather(X,Y).
sameparent(X,Y) :- samemother(X,Y), not(samefather(X,Y)).


brother(X,Y) :-
	male(X), sameparent(X,Y), not(X=Y).

sister(X, Y) :-
    female(X),
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

sibling(X,Y) :- samefather(X,Y), not(X=Y).
sibling(X,Y) :- samemother(X,Y), not(samefather(X,Y)), not(X=Y).

uncle(X, Y) :-
    % Case (a): X is the brother of Y's father or mother
    parent(Z, Y),
    brother(X, Z).

uncle(X, Y) :-
    % Case (b): X is married to the sister of Y's father or mother
    parent(Z, Y),
    sister(W, Z),
    married(X, W).

descendant(X,Y) :-
	% Case (a) X is a child of Y
	parent(Y, X).

descendant(X, Y) :-
	% Case (b) X is a child of Z who is a descendent of Y.
	parent(Z, X),
	descendant(Z, Y).

brotherInLaw(X, Y) :-
    % Case (a) X is the brother of Y's husband or wife
    male(X),
    married(Y, Z),
    sibling(X, Z).

brotherInLaw(X, Y) :-
    % Case (b) X is the husband of Y's sister
    male(X),
    married(X, Z),
    sibling(Y, Z),
    female(Z).

sisterInLaw(X, Y) :-
    % X is the sister of Y's husband or wife
    female(X),
    married(Y, Z),
    sibling(X, Z).

sisterInLaw(X, Y) :-
    % X is the wife of Y's brother
    female(X),
    married(X, Z),
    sibling(Y, Z),
    male(Z).

niece(X, Y) :-
    % X is the daughter of Y's brother or sister
    female(X),
    parent(Z, X),
    sibling(Z, Y).

niece(X, Y) :-
    % X is the daughter of Y's brother-in-law (husband of Y's sister)
    female(X),
    parent(Z, X),
    sisterInLaw(Z, Y).

niece(X, Y) :-
    % X is the daughter of Y's sister-in-law (wife of Y's brother)
    female(X),
    parent(Z, X),
    brotherInLaw(Z, Y).

makeList(0, _, []).
makeList(N, E, [E|T]) :-
	% true if L is a list consists N (first parameter, non-negative) copies of the object e (2nd parameter)
	% Ex. ?-    makeList(4, siue, L).
	% L = [siue, siue, siue, siue].
	N > 0,
	N1 is N - 1,
	makeList(N1, E, T).

level([], []).

level([Head|Tail], [Head|FlatTail]) :-
    \+ is_list(Head), % Check if Head is not a list.
    level(Tail, FlatTail). % Flatten the tail.

level([Head|Tail], FlatList) :-
    is_list(Head), % Check if Head is a list.
    level(Head, FlatHead), % Flatten the head.
    level(Tail, FlatTail), % Flatten the tail.
    append(FlatHead, FlatTail, FlatList). % Concatenate the flattened head and tail.

remove1(_, [], []).

remove1(E, [E|Tail], L2) :-
    remove1(E, Tail, L2).

remove1(E, [Head|Tail], [Head|NewTail]) :-
    E \= Head, 
    remove1(E, Tail, NewTail).

removeAll(_, [], []).

removeAll(E, [E|Tail], L2) :-
    removeAll(E, Tail, L2).

removeAll(E, [Head|Tail], [NewHead|NewTail]) :-
    is_list(Head), % Check if Head is a list.
    removeAll(E, Head, NewHead), % Recursively remove E from the sublist.
    !, 
    removeAll(E, Tail, NewTail).

removeAll(E, [Head|Tail], [Head|NewTail]) :-
    E \= Head, % Check if Head is not the element to be removed.
    removeAll(E, Tail, NewTail).














