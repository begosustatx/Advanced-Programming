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
follows(G, X, Y) :- memberPerson(person(X, F), G), 
                    memberPerson(Y, F). 

% different(G, X, Y) - HELPER FUNCTION
different(_, _, []).
different(G, X, [H|T]) :- selectPerson(G, person(X, _), G1), 
                          selectPerson(G1, person(H, _), _), 
                          different(G, X, T). 

% ignores(G, X, Y)
ignores(G, X, Y) :- follows(G, Y, X), 
                    memberPerson(person(X, F), G), 
                    different(G, Y, F).

%%% level 1 %%%

% mutualFollow(G, X, L) - HELPER FUNCTION
mutualFollow(_, _, []).
mutualFollow(G, X, [H|T]) :- follows(G, X, H), 
                             follows(G, H, X), 
                             mutualFollow(G, X, T). 

% popular(G, X)
popular(G, X) :- memberPerson(person(X, Y), G), 
                 mutualFollow(G, X, Y).

% ignoresFollow(G, X, L) - HELPER FUNCTION
ignoresFollow(_, _, []).
ignoresFollow(G, X, [H|T]) :- ignores(G, H, X), 
                         ignoresFollow(G, X, T). 

% outcast(G, X)
outcast(G, X) :- memberPerson(person(X, F), G), 
                 ignoresFollow(G, X, F).

% followers(G, X, NG, FS) - HELPER FUNCTION
followers(_, _, [], []).
followers(G, X, [person(N, _)|T], FS) :- follows(G, N, X),
                                         appendPerson([N], FF, FS),
                                         followers(G, X, T, FF).

followers(G, X, [person(_, F)|T], FS) :- different(G, X, F), 
                                         followers(G, X, T, FS).

% checkFollowers(XF,FS) - HELPER FUNCTION
checkFollowers([_|_], []).
checkFollowers([], []).
checkFollowers(F, [H|T]) :- memberPerson(H, F),
                            checkFollowers(F, T).

% friendly(G, X)
friendly(G, X) :- memberPerson(person(X, XF), G), 
                  followers(G, X, G, FS), 
                  checkFollowers(XF, FS).

% checkIgnores(G, XF,FS) - HELPER FUNCTION
checkIgnores(_, [], _).
checkIgnores(G, [H|T], FS) :- different(G, H, FS),
                              checkIgnores(G, T, FS).

% hostile(G, X)
hostile(G, X) :- followers(G, X, G, FS), 
                 memberPerson(person(X, XF), G), 
                 checkIgnores(G, XF, FS).

%%% level 2 %%%

% aware(G, X, Y)
aware(G, X, Y) :- follows(G, X, Y).
aware(G, X, Y) :- different(G, X, [Y]), 
                  follows(G, X, Z), 
                  selectPerson(G, person(X, _), G1), 
                  aware(G1, Z, Y).

% awareness(G, X, Y, L) - HELPER FUNCTION
awareness(_, [], Y, Y).
awareness(G, [H|T], Y, L) :- memberPerson(H, Y), 
                             awareness(G, T, Y, L).

awareness(G, [H|T], Y, L) :- different(G, H, Y), 
                             appendPerson(Y, [H], NY), 
                             memberPerson(person(H, F), G),
                             appendPerson(T, F, NX), 
                             awareness(G, NX, NY, L).

% ignorant(G, X, Y)
ignorant(G, X, Y) :-  different(G,X,[Y]), 
                      awareness(G, [X], [], L), 
                      selectPerson(L, X, LL), 
                      different(G, Y, LL).

%%% level 3 %%%

% permutationWorld(G, H) - HELPER FUNCTION
permutationWorld([],[]).
permutationWorld([H|T], S) :- permutationWorld(T, P), appendPerson(X, Y, P), appendPerson(X, [H|Y], S).

% makePersonPair(G, H, K) - HELPER FUNCTION
makePersonPair([person(G, _)], [person(H, _)], [p(G,H)]).
makePersonPair([person(G, _)|T], [person(H, _)|TT], K) :- appendPerson([p(G,H)], KK, K), makePersonPair(T, TT, KK).

% findPersonName(X, K, Y) - HELPER FUNCTION
findPersonName(X, K, Y) :- memberPerson(p(X,Y), K).

% findNewFollower(XF, K, L) - HELPER FUNCTION
findNewFollower([], _, []).
findNewFollower([H|T], K, L) :- findPersonName(H, K, Y), 
                                appendPerson([Y], LL, L), 
                                findNewFollower(T, K, LL).

% replaceGraph(X, K, G) - HELPER FUNCTION
replaceGraph([], _, []).
replaceGraph([person(X, XF)| T], K, G) :- findPersonName(X, K, Y),
                                          findNewFollower(XF, K, L),        
                                          appendPerson([person(Y, L)], NG, G), 
                                          replaceGraph(T, K, NG).   

% compareGraph(H, F) - HELPER FUNCTION
compareGraph([], _).
compareGraph([person(N,L)|T], F) :- permutationWorld(L, NL),
                                    memberPerson(person(N,NL), F),
                                    compareGraph(T, F). 

% same_world(G, H, K)
same_world([], [], []).
same_world(G, H, K) :- permutationWorld(H, HH),
                       makePersonPair(G, HH, K),
                       replaceGraph(G, K, NG),
                       compareGraph(NG, H).

% optional!
% different_world(G, H)
