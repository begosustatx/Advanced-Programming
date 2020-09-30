% Rudimentary test suite. Feel free to replace anything

% Can run as: swipl -g run_tests,halt src/instahub.pl tests/instatest.pl

% The sample graphs from the assignment text:
g1([person(kara, [barry, clark]),
    person(bruce,[clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

g2([person(batman, [green_arrow, superman]),
    person(green_arrow, [supergirl]),
    person(supergirl, [flash, superman]),
    person(flash, [green_arrow, supergirl]),
    person(superman, [green_arrow, supergirl])]).

:- begin_tests(instahub).

% myMember(X, L)
test(myMember1, (nondet)) :-
    myMember(bruce, [clark,oliver,bruce]).

test(myMember2, (fail)) :-
    myMember(bruce, [clark,oliver]).

test(myMember3, (fail)) :-
    myMember(bruce, []).

test(myMember4, [set(X==[barry,oliver,bruce,kara])]) :-
    myMember(X, [barry,oliver,bruce,kara]).

% myAppend(L1, L2, L3)
test(myAppend1, (nondet)) :-
    myAppend([clark], [oliver,bruce],[clark,oliver,bruce]).

test(myAppend2, (nondet)) :-
    myAppend([], [oliver,bruce],[oliver,bruce]).

test(myAppend3, (nondet)) :-
    myAppend([oliver,bruce],[],[oliver,bruce]).

test(myAppend4, (nondet)) :-
    myAppend([],[],[]).

test(myAppend5, (fail)) :-
    myAppend([oliver,bruce,mark],[],[oliver,bruce]).

test(myAppend6, (fail)) :-
    myAppend([oliver,bruce],[],[mark,oliver,bruce]).

test(myAppend7, (fail)) :-
    myAppend([oliver,bruce],[mark],[oliver,bruce]).

test(myAppend8, X=[barry,oliver,bruce,kara]) :-
    myAppend([barry,oliver,bruce],[kara],X).

% mySelect(L1, X, L2)
test(mySelect1, (nondet)) :-
    mySelect([clark,oliver], oliver,[clark]).

test(mySelect2, (fail)) :-
    mySelect([mark,oliver], oliver,[mark,oliver]).

test(mySelect3, (nondet)) :-
    mySelect([mark], mark,[]).

test(mySelect4, (fail)) :-
    mySelect([mark], oliver,[]).

% follows(G, X, Y)
test(follows1, [nondet]) :-
    g1(G), follows(G, bruce, clark).

test(follows2, [fail]) :-
    g1(G), follows(G, clark, bruce).

test(follows3, [set(X == [barry,clark,oliver])]) :-
    g1(G), follows(G, X, kara).

test(follows4, [fail]) :-
    g1(G), follows(G, _, bruce).

test(follows5, [set(Y == [barry,clark])]) :-
    g1(G), follows(G, kara, Y).

test(follows5, [set(Y == [barry,clark])]) :-
    g1(G), follows(G, kara, Y).

test(follows6, [set(Y == [barry, clark])]) :-
    g1(G), follows(G, kara, Y).

test(follows7, [set(X == [barry, clark,oliver])]) :-
    g1(G), follows(G, X, kara).

% different(G, X, Y)
test(different1, [nondet]) :-
    g1(G), different(G, clark, [bruce]).

test(different2, [fail]) :-
     g1(G), different(G, clark,[clark]).

test(different3, [set(X == [kara, bruce, barry,oliver])]) :-
     g1(G), different(G, X,[clark]).

% ignores(G, X, Y)
test(ignores1, [nondet]) :-
    g1(G), ignores(G, clark, bruce).

test(ignores2, [fail]) :-
    g1(G), ignores(G, bruce, clark).

test(ignores3, [nondet]) :-
    g1(G), ignores(G, kara, oliver).

test(ignores4, [fail]) :-
    g1(G), ignores(G, oliver, kara).

test(ignores5, [set(Y == [oliver])]) :-
    g1(G), ignores(G, kara, Y).

test(ignores6, [fail]) :-
    g1(G), ignores(G, _Y, kara).

test(ignores7, [set(Y == [kara])]) :-
    g1(G), ignores(G, Y, oliver).

% mutualFollow(G, X, L)
test(mutualFollow1, [nondet]) :-
    g1(G), mutualFollow(G, kara, [barry,clark]).

test(mutualFollow2, [fail]) :-
    g1(G), mutualFollow(G, kara, [oliver,clark]).

test(mutualFollow3, [set( X==[barry,clark])]) :-
    g1(G), mutualFollow(G, X, [kara]).

test(mutualFollow4, [fail]) :-
    g1(G), mutualFollow(G, _X, [oliver]).

% popular(G, X)
test(popular1, [fail]) :-
    g1(G), popular(G, oliver).

test(popular2, [nondet]) :-
    g1(G), popular(G, kara).

test(popular3, [set( X==[kara])]) :-
    g1(G), popular(G, X).

% ignoresFollow(G, X, L)
test(ignoresFollow1, [fail]) :-
    g1(G), ignoresFollow(G, oliver,[clark,barry,bruce]).

% outcast(G, X)
test(outcast1, [nondet]) :-
    g1(G), outcast(G, oliver).

test(outcast2, [fail]) :-
    g1(G), outcast(G, kara).

test(outcast3, [set( Y ==[bruce,oliver])]) :-
    g1(G), outcast(G, Y).

% followers(G, X, NG, FS)
test(followers1, [nondet]) :-
    g1(G), followers(G, kara,G,[barry, clark, oliver]).

test(followers2, [fail]) :-
    g1(G), followers(G, bruce,G,[kara]).

test(followers3, [set( X==[oliver])]) :-
    g1(G), followers(G, X ,G, [bruce, barry, clark]).

% checkFollowers(L1,L2)
test(checkFollowers1, [nondet]) :-
    checkFollowers([bruce, barry, clark],[barry, clark]).

test(checkFollowers2, [fail]) :-
    checkFollowers([clark],[barry, clark]).

% friendly(G, X)
test(friendly1, [nondet]) :-
    g1(G), friendly(G,barry).

test(friendly2, [fail]) :-
    g1(G), friendly(G,kara).

test(friendly2, [set( X==[barry,bruce])]) :-
    g1(G), friendly(G,X).

% checkIgnores(G, XF,FS)
test(checkIgnores1, [nondet]) :-
    g1(G), checkIgnores(G,[kara],[barry,clark]).

test(checkIgnores2, [fail]) :-
    g1(G), checkIgnores(G,[kara],[kara,oliver]).

% hostile(G, X)
test(hostile1, [nondet]) :-
    g1(G), hostile(G,oliver).

test(hostile2, [fail]) :-
    g1(G), hostile(G,barry).

% aware(G, X, Y)
test(aware1, [nondet]) :-
    g1(G), aware(G,bruce,kara).

test(aware2, [fail]) :-
    g1(G), aware(G,kara,bruce).

% awareness(G,G,X,L) %
test(awareness1, [fail]) :-
    g1(G), awareness(G,[bruce],[],[kara, barry, clark, oliver]).

test(awareness2, [fail]) :-
    g1(G), awareness(G,[kara],[],[]).

% ignorant(G, X, Y)
test(ignorant1, [fail]) :-
    g1(G), ignorant(G, bruce, bruce).

test(ignorant2, [nondet]) :-
    g1(G), ignorant(G, kara, bruce).

% permutationWorld(L1,L2).
test(permutationWorld1, [nondet]) :-
    permutationWorld([kara,bruce], [kara,bruce]).

test(permutationWorld2, [nondet]) :-
    permutationWorld([kara,bruce], [bruce,kara]).

% makePersonPair(G, H, K) 
test(makePersonPair1, [nondet]) :-
    g1(G),g2(H),makePersonPair(G,H,[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)] ).

test(makePersonPair1, [fail]) :-
    g1(G),g2(H),makePersonPair(G,H,[] ).

% findPersonName(X,K,Y)
test(findPersonName1, [nondet]) :-
    findPersonName(kara,[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],batman ).

test(findPersonName1, [fail]) :-
    findPersonName(kara,[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],superman).

% findNewFollower(K,X,L)

% replaceGraph (H,K,F)

:- end_tests(instahub).