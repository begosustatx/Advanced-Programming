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

test(follows6, [set(X == [kara, kara, bruce, bruce, barry, barry, clark, clark, oliver]), 
                set(Y == [barry, clark, clark, oliver, kara, oliver, oliver, kara, kara])]) :-
    g1(G), follows(G, X, Y).

test(ignores1, [nondet]) :-
    g1(G), ignores(G, clark, bruce).

test(ignores2, [fail]) :-
    g1(G), ignores(G, bruce, clark).

test(ignores3, [nondet]) :-
    g1(G), ignores(G, kara, oliver).

test(ignores4, [fail]) :-
    g1(G), ignores(G, oliver, kara).

:- end_tests(instahub).
