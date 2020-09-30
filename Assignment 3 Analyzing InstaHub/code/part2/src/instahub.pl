% Advanced Programming Assignment 3
% Skeleton for main part. Predicates to implement:

% myMember(X, L) - HELPER FUNCTION
myMember(X, [X| _]).
myMember(X, [_| X1]) :- myMember(X, X1).

% myAppend(L1, L2, L3) - HELPER FUNCTION 
myAppend([], L2, L2).
myAppend([H|L1], L2, [H|L3]) :- myAppend(L1, L2, L3).

% mySelect(L1, X, L2) - HELPER FUNCTION
mySelect([X|T], X, T).
mySelect([H|T], X, [H|S]) :- mySelect(T, X, S).

%%% level 0 %%%

% follows(G, X, Y)
follows(G, X, Y) :- myMember(person(X, F), G), 
                    myMember(Y, F). 

% different(G, X, Y) - HELPER FUNCTION
different(_, _, []).
different(G, X, [H|T]) :- mySelect(G, person(X, _), G1), 
                          mySelect(G1, person(H, _), _), 
                          different(G, X, T). 

% ignores(G, X, Y)
ignores(G, X, Y) :- follows(G, Y, X), 
                    myMember(person(X, F), G), 
                    different(G, Y, F).

%%% level 1 %%%

% mutualFollow(G, X, L) - HELPER FUNCTION
mutualFollow(_, _, []).
mutualFollow(G, X, [H|T]) :- follows(G, X, H), 
                             follows(G, H, X), 
                             mutualFollow(G, X, T). 

% popular(G, X)
popular(G, X) :- myMember(person(X, Y), G), 
                 mutualFollow(G, X, Y).

% ignoresFollow(G, X, L) - HELPER FUNCTION
ignoresFollow(_, _, []).
ignoresFollow(G, X, [H|T]) :- ignores(G, H, X), 
                             ignoresFollow(G, X, T). 

% outcast(G, X)
outcast(G, X) :- myMember(person(X, F), G), 
                 ignoresFollow(G, X, F).

% followers(G, X, NG, FS) - HELPER FUNCTION
followers(_, _, [], []).
followers(G, X, [person(N, _)|T], FS) :- follows(G, N, X),
                                         myAppend([N], FF, FS),
                                         followers(G, X, T, FF).

followers(G, X, [person(_, F)|T], FS) :- different(G, X, F), 
                                         followers(G, X, T, FS).

% checkFollowers(XF,FS) - HELPER FUNCTION
checkFollowers([_|_], []).
checkFollowers([], []).
checkFollowers(F, [H|T]) :- myMember(H, F),
                            checkFollowers(F, T).

% friendly(G, X)
friendly(G, X) :- myMember(person(X, XF), G), 
                  followers(G, X, G, FS), 
                  checkFollowers(XF, FS).

% checkIgnores(G, XF,FS) - HELPER FUNCTION
checkIgnores(_, [], _).
checkIgnores(G, [H|T], FS) :- different(G, H, FS),
                              checkIgnores(G, T, FS).

% hostile(G, X)
hostile(G, X) :- followers(G, X, G, FS), 
                 myMember(person(X, XF), G), 
                 checkIgnores(G, XF, FS).

%%% level 2 %%%

% aware(G, X, Y)
aware(G, X, Y) :- follows(G, X, Y).
aware(G, X, Y) :- different(G, X, [Y]), 
                  follows(G, X, Z), 
                  mySelect(G, person(X, _), G1), 
                  aware(G1, Z, Y).

% awareness(G, X, Y, L) - HELPER FUNCTION
awareness(_, [], Y, Y).
awareness(G, [H|T], Y, L) :- myMember(H, Y), 
                             awareness(G, T, Y, L).

awareness(G, [H|T], Y, L) :- different(G, H, Y), 
                             myAppend(Y, [H], NY), 
                             myMember(person(H, F), G),
                             myAppend(T, F, NX), 
                             awareness(G, NX, NY, L).

% ignorant(G, X, Y)
ignorant(G, X, Y) :-  different(G,X,[Y]), 
                      awareness(G, [X], [], L), 
                      mySelect(L, X, LL), 
                      different(G, Y, LL).

%%% level 3 %%%

% myPermutation(G, H) - HELPER FUNCTION
myPermutation([],[]).
myPermutation([H|T], S) :- myPermutation(T, P), myAppend(X, Y, P), myAppend(X, [H|Y], S).

% makePersonPair(G, H, K) - HELPER FUNCTION
makePersonPair([person(G, _)], [person(H, _)], [p(G,H)]).
makePersonPair([person(G, _)|T], [person(H, _)|TT], K) :- myAppend([p(G,H)], KK, K), makePersonPair(T, TT, KK).

% findPersonName(X, K, Y) - HELPER FUNCTION
findPersonName(X, K, Y) :- myMember(p(X,Y), K).

% findNewFollower(XF, K, L) - HELPER FUNCTION
findNewFollower([], _, []).
findNewFollower([H|T], K, L) :- findPersonName(H, K, Y), 
                                myAppend([Y], LL, L), 
                                findNewFollower(T, K, LL).

% replaceGraph(X, K, G) - HELPER FUNCTION
replaceGraph([], _, []).
replaceGraph([person(X, XF)| T], K, G) :- findPersonName(X, K, Y),
                                          findNewFollower(XF, K, L),        
                                          myAppend([person(Y, L)], NG, G), 
                                          replaceGraph(T, K, NG).   

% compareGraph(H, F) - HELPER FUNCTION
compareGraph([], _).
compareGraph([person(N,L)|T], F) :- myPermutation(L, NL),
                                    myMember(person(N,NL), F),
                                    compareGraph(T, F). 

% same_world(G, H, K)
same_world([], [], []).
same_world(G, H, K) :- myPermutation(H, HH),
                       makePersonPair(G, HH, K),
                       replaceGraph(G, K, NG),
                       compareGraph(NG, H).

% optional!
% different_world(G, H)
