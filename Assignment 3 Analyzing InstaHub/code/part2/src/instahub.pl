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

g([person(kara, [barry, clark]), person(bruce, [clark, oliver]), 
person(barry, [kara, oliver]), person(clark, [oliver, kara]), 
person(oliver, [kara])]).

%%% level 0 %%%

% follows(G, X, Y)
follows(G, X, Y) :- memberPerson(person(X, F), G), memberPerson(Y, F). 

% different(G, X, Y) - HELPER FUNCTION
different(G, X, []).
different(G, X, [H|T]) :- selectPerson(G, person(X, _), G1), selectPerson(G1, person(H, _), _), different(G, X, T). 

% ignores(G, X, Y)
ignores(G, X, Y) :- follows(G, Y, X), memberPerson(person(X, X1), G), different(G, Y, X1).

%%% level 1 %%%

% mutualFollow(G, X, L) - HELPER FUNCTION
mutualFollow(G, X, []).
mutualFollow(G, X, [H|T]) :- follows(G, X, H), follows(G, H, X), mutualFollow(G, X, T). 

% popular(G, X)
popular(G, X) :- memberPerson(person(X, Y), G), mutualFollow(G, X, Y).

% outcast(G, X)

% friendly(G, X)

% hostile(G, X)

%%% level 2 %%%

% aware(G, X, Y)

% ignorant(G, X, Y)

%%% level 3 %%%

% same_world(G, H, K)

% optional!
% different_world(G, H)
