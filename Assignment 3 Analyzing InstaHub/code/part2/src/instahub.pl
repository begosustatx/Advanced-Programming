% Advanced Programming Assignment 3
% Skeleton for main part. Predicates to implement:

% memberPerson(X, L) - HELPER FUNCTION
memberPerson(X, [X| _]).
memberPerson(X, [_| X1]) :- memberPerson(X, X1).

% appendPerson(L1, L2, L3) - HELPER FUNCTION 
appendPerson([], L2, L2).
appendPerson([H|L1], L2, [H|L3]) :- appendPerson(L1, L2, L3).

% selectPerson(L1, X, L2) - HELPER FUNCTION
selectPerson([X|T], X, T).
selectPerson([H|T], X, [H|S]) :- selectPerson(T, X, S).

%%% level 0 %%%

% follows(G, X, Y)
follows(G, X, Y) :- memberPerson(person(X, F), G), memberPerson(Y, F). 

% different(G, X, Y) - HELPER FUNCTION
different(_, _, []).
different(G, X, [H|T]) :- selectPerson(G, person(X, _), G1), selectPerson(G1, person(H, _), _), different(G, X, T). 

% ignores(G, X, Y)
ignores(G, X, Y) :- follows(G, Y, X), memberPerson(person(X, X1), G), different(G, Y, X1).

%%% level 1 %%%

% mutualFollow(G, X, L) - HELPER FUNCTION
mutualFollow(_, _, []).
mutualFollow(G, X, [H|T]) :- follows(G, X, H), follows(G, H, X), mutualFollow(G, X, T). 

% popular(G, X)
popular(G, X) :- memberPerson(person(X, Y), G), mutualFollow(G, X, Y).

% unFollow(G, X, L) - HELPER FUNCTION
unFollow(_, _, []).
unFollow(G, X, [H|T]) :- ignores(G, H, X), unFollow(G, X, T). 

% outcast(G, X)
outcast(G, X) :- memberPerson(person(X, X1), G), unFollow(G, X, X1).

% friendly(G, X)
friendly([], _).
friendly(G, X) :- follows(G, Y, X), selectPerson(G, person(Y, _), G1), follows(G, X, Y), friendly(G1,X).

% hostile(G, X)
hostile([], _).
hostile(G, X) :- memberPerson(person(Y,_), G), selectPerson(G, person(Y, _), G1), ignores(G, Y, X), follows(G, X, Y), hostile(G1, X).

%%% level 2 %%%

% chainFollow(G, L, Y) - HELPER FUNCTION
chainFollow(G, [H|T], Y) :- follows(G, H, Y), chainFollow(G, T, Y). 

% aware(G, X, Y)
aware(G, X, Y) :- follows(G, X, Y).
aware(G, X, Y) :- memberPerson(person(X, L), G), chainFollow(G, L, Y).

% ignorant(G, X, Y)
ignorant(_, _, _).

%%% level 3 %%%

% same_world(G, H, K)
same_world(G, H, K) :-  selectPerson(G, person(X, _), G1), selectPerson(H, person(Y, _), H1), appendPerson([p(X,Y)], [], K), same_world(G1, H1, K).

% optional!
% different_world(G, H)
