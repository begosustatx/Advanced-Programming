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

% followers(G, X, NG, FS) - HELPER FUNCTION
followers(G, X, [], []).
followers(G, X, [person(N, F)|T], FS) :- 
    follows(G, N, X),
    appendPerson([N], FF, FS),
    followers(G, X, T, FF).
followers(G, X, [person(N, F)|T], FS) :- 
    different(G, X, F), 
    followers(G, X, T, FS).

% personFollowers(XF,FS) - HELPER FUNCTION
personFollowers([_|T], []).
personFollowers([], []).
personFollowers(F, [H|T]) :-
    memberPerson(H, F),
    personFollowers(F, T).

% friendly(G, X)
friendly(G, X) :- memberPerson(person(X, XF), G), followers(G, X, G, FS), personFollowers(XF, FS).

% checkIgnores(G, XF,FS) - HELPER FUNCTION
checkIgnores(G, [], FS).
checkIgnores(G, [H|T], FS) :-
    different(G, H, FS),
    checkIgnores(G, T, FS).

% hostile(G, X)
hostile(G, X) :- followers(G, X, G, FS), memberPerson(person(X, XF), G), checkIgnores(G, XF, FS).

%%% level 2 %%%

% aware(G, X, Y)
aware(G, X, Y) :- follows(G, X, Y).
aware(G, X, Y) :- different(G, X, [Y]), follows(G, X, Z), selectPerson(G, person(X, _), G1), aware(G1, Z, Y).

% awareness(G, X, NG, L)
awareness(_, X, [], []).
awareness(G, X, [person(N, _)|T], L) :- aware(G, X, N), appendPerson([N], FF, L), awareness(G, X, T, FF).
awareness(G, X, [person(N, F)|T], L) :- different(G, X, F), awareness(G, X, T, L).

% ignorant(G, X, Y)
ignorant(G, X, Y) :- awareness(G, X, G, L), different(G, Y, L).

%%% level 3 %%%

% permutationWorld(G, H)
permutationWorld([],[]).
permutationWorld([H|T], S) :- permutationWorld(T, P), appendPerson(X, Y, P), appendPerson(X, [H|Y], S).

% makePair(G, H, K) - HELPER FUNCTION
makePair([person(G, _)], [person(H, _)], [p(G,H)]).
makePair([person(G, _)|T], [person(H, _)|TT], K) :- appendPerson([p(G,H)], KK, K), makePair(T, TT, KK).

% findPersonName(X, K, Y)
findPersonName(X, K, Y) :- memberPerson(p(X,Y), K).

% findNewFollower(XF, K, L)
findNewFollower([], K, []).
findNewFollower([H|T], K, L) :- 
    findPersonName(H, K, Y), 
    appendPerson([Y], LL, L), 
    findNewFollower(T, K, LL).

% replaceGraph(X, K, G)
replaceGraph([], _, []).
replaceGraph([person(X, XF)| T], K, G) :- findPersonName(X, K, Y),
                                      findNewFollower(XF, K, L),        
                                      appendPerson([person(Y, L)], NG, G), 
                                      replaceGraph(T, K, NG).   
% compareGraph(H, F)
compareGraph([], _).
compareGraph([person(N,L)|T], F) :- permutationWorld(L, NL),
                                    memberPerson(person(N,NL), F),
                                    compareGraph(T, F). 

% same_world(G, H, K)
same_world([], [], []).
same_world(G, H, K) :- permutationWorld(H, HH),
                       makePair(G, HH, K),
                       replaceGraph(G, K, NG),
                       compareGraph(NG, H).

% optional!
% different_world(G, H)
