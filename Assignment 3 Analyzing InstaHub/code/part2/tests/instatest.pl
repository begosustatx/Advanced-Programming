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

b1([person(jeff, [bill, mark]),
    person(bill,[steve, jeff]),
    person(mark, [jeff, bill, elon, larry, sergey]),
    person(bernard, [bill, jeff]),
    person(mukesh, []),
    person(steve, [bill]),
    person(warren, [mukesh, bernard]),
    person(larry, [sergey]),
    person(elon, [jeff]),
    person(sergey, [larry])]).

b2([person(amazon, [microsoft, facebook]),
    person(microsoft,[clippers, amazon]),
    person(facebook, [amazon, microsoft, tesla, google, alphabet]),
    person(sephora , [microsoft, amazon]),
    person(reliance , []),
    person(clippers, [microsoft]),
    person(berkshire, [reliance, sephora]),
    person(google, [alphabet]),
    person(tesla, [amazon]),
    person(alphabet, [google])]).

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

test(myMember5, [set(X==[amazon,microsoft,facebook,sephora,reliance,clippers,berkshire,google, tesla, alphabet])]) :-
    b2(G), myMember(person(X,_), G).

test(myMember6, [nondet]) :-
    b1(G), myMember(person(_,[jeff, bill, elon, larry, sergey]), G).

test(myMember7, [fail]) :-
    b1(G), myMember(person(jack,_), G).

test(myMember8, [fail]) :-
    b2(G), myMember(person(_,[bill, elon]), G).

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

test(mySelect5, [set(X==[[bill,mark],[steve, jeff], [jeff,bill,elon,larry,sergey], [bill,jeff], [], [bill],
                         [mukesh,bernard], [sergey], [jeff], [larry]])]) :-
    b1(G), mySelect(G, person(_,X), _G1).

test(mySelect6, [set(X==[amazon,microsoft,facebook,sephora,reliance,clippers,berkshire,google, tesla, alphabet])]) :-
    b2(G), mySelect(G, person(X,_), _G1).

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

test(follows8, [fail]) :-
    b1(G), follows(G, mukesh, _Y).

test(follows8, [set(X == [jeff, bill, elon, larry, sergey])]) :-
    b1(G), follows(G, mark, X).

% different(G, X, Y)
test(different1, [nondet]) :-
    g1(G), different(G, clark, [bruce]).

test(different2, [fail]) :-
     g1(G), different(G, clark,[clark]).

test(different3, [set(X == [kara, bruce, barry,oliver])]) :-
     g1(G), different(G, X,[clark]).

test(different4, [fail]) :-
     b1(G), different(G, bill,[mark, jeff, elon, bill]).

test(different5, [nondet]) :-
     b1(G), different(G, bill,[mark, jeff, elon, larry, sergey]).

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

test(ignores8, [nondet]) :-
    b1(G), ignores(G, jeff, elon).

test(ignores9, [set(Y == [mark, bernard])]) :-
    b1(G), ignores(G, bill, Y).

test(ignores10, [set(X == [microsoft, tesla, google, alphabet])]) :-
   b2(G), ignores(G, X, facebook).

% mutualFollow(G, X, L)
test(mutualFollow1, [nondet]) :-
    g1(G), mutualFollow(G, kara, [barry,clark]).

test(mutualFollow2, [fail]) :-
    g1(G), mutualFollow(G, kara, [oliver,clark]).

test(mutualFollow3, [set( X==[barry,clark])]) :-
    g1(G), mutualFollow(G, X, [kara]).

test(mutualFollow4, [fail]) :-
    g1(G), mutualFollow(G, _X, [oliver]).

test(mutualFollow4, [set( X==[jeff,steve])]) :-
    b1(G), mutualFollow(G, X, [bill]).

test(mutualFollow4, [nondet]) :-
    b2(G), mutualFollow(G, microsoft, [amazon,clippers]).    

% popular(G, X)
test(popular1, [fail]) :-
    g1(G), popular(G, oliver).

test(popular2, [nondet]) :-
    g1(G), popular(G, kara).

test(popular3, [set( X==[kara])]) :-
    g1(G), popular(G, X).

test(popular4, [nondet]) :-
    b1(G), popular(G, jeff).

test(popular5, [set( X==[jeff, bill, mukesh, steve, larry, sergey])]) :-
    b1(G), popular(G, X).

test(popular6, [fail]) :-
    b2(G), popular(G, tesla).

% ignoresFollow(G, X, L)
test(ignoresFollow1, [fail]) :-
    g1(G), ignoresFollow(G, oliver,[clark,barry,bruce]).

test(ignoresFollow3, [nondet]) :-
    b1(G), ignoresFollow(G, elon, [jeff]).

% outcast(G, X)
test(outcast1, [nondet]) :-
    g1(G), outcast(G, oliver).

test(outcast2, [fail]) :-
    g1(G), outcast(G, kara).

test(outcast3, [set( Y ==[bruce,oliver])]) :-
    g1(G), outcast(G, Y).

test(outcast4, [set( Y ==[bernard,mukesh,warren,elon])]) :-
    b1(G), outcast(G, Y).

test(outcast5, [nondet]) :-
    b2(G), outcast(G, tesla).

test(outcast6, [fail]) :-
    b1(G), outcast(G, bill).

% followers(G, X, NG, FS)
test(followers1, [nondet]) :-
    g1(G), followers(G, kara,G,[barry, clark, oliver]).

test(followers2, [fail]) :-
    g1(G), followers(G, bruce,G,[kara]).

test(followers3, [set( X==[oliver])]) :-
    g1(G), followers(G, X ,G, [bruce, barry, clark]).

test(followers4, [set(Y==[[bill,mark,bernard,elon]])]) :-
    b1(G), followers(G, jeff ,G, Y).

test(followers5, [fail]) :-
    b1(G), followers(G, jeff ,G, [bill,mark,bernard]).

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

test(friendly3, [set( X==[barry,bruce])]) :-
    g1(G), friendly(G,X).

test(friendly4, [nondet]) :-
    b1(G), friendly(G,warren).

test(friendly5, [fail]) :-
    b1(G), friendly(G,bill).

test(friendly6, [set( X==[facebook,clippers,berkshire])]) :-
    b2(G), friendly(G,X).

% checkIgnores(G, XF,FS)
test(checkIgnores1, [nondet]) :-
    g1(G), checkIgnores(G,[kara],[barry,clark]).

test(checkIgnores2, [fail]) :-
    g1(G), checkIgnores(G,[kara],[kara,oliver]).

test(checkIgnores3, [nondet]) :-
    b2(G), checkIgnores(G,[google],[facebook]).

test(checkIgnores4, [fail]) :-
    b2(G), checkIgnores(G,[microsoft],[microsoft]).

% hostile(G, X)
test(hostile1, [nondet]) :-
    g1(G), hostile(G,oliver).

test(hostile2, [fail]) :-
    g1(G), hostile(G,barry).

test(hostile3, [nondet]) :-
    b1(G), hostile(G,mukesh).

test(hostile4, [fail]) :-
    b2(G), hostile(G,google).

% aware(G, X, Y)
test(aware1, [nondet]) :-
    g1(G), aware(G,bruce,kara).

test(aware2, [fail]) :-
    g1(G), aware(G,kara,bruce).

test(aware3, [nondet]) :-
    b1(G), aware(G,bill,steve).

test(aware4, [fail]) :-
    b1(G), aware(G,bill,mukesh).

% awareness(G,G,X,L) %
test(awareness1, [nondet]) :-
    g1(G), awareness(G,[bruce],[],[bruce,clark,oliver,kara,barry]).

test(awareness2, [fail]) :-
    g1(G), awareness(G,[kara],[],[]).

test(awareness3, [nondet]) :-
    b2(G), awareness(G,[microsoft],[],[microsoft,clippers,amazon,facebook,tesla,google,alphabet]).

test(awareness4, [fail]) :-
    b2(G), awareness(G,[microsoft],[],[]).

% ignorant(G, X, Y)
test(ignorant1, [fail]) :-
    g1(G), ignorant(G, bruce, bruce).

test(ignorant2, [nondet]) :-
    g1(G), ignorant(G, kara, bruce).

test(ignorant1, [fail]) :-
    b2(G), ignorant(G, tesla, tesla).

test(ignorant2, [nondet]) :-
    b2(G), ignorant(G, microsoft, sephora).

% myPermutation(L1,L2).
test(myPermutation1, [nondet]) :-
    myPermutation([kara,bruce], [kara,bruce]).

test(myPermutation2, [nondet]) :-
    myPermutation([kara,bruce], [bruce,kara]).

test(myPermutation1, [nondet]) :-
    myPermutation([microsoft,tesla,amazon], [amazon,microsoft, tesla]).

test(myPermutation2, [nondet]) :-
    myPermutation([], []).

test(myPermutation2, [fail]) :-
    myPermutation([1,2,3], [2,3]).

% makePersonPair(G, H, K) 
test(makePersonPair1, [nondet]) :-
    g1(G),g2(H),makePersonPair(G,H,[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)]).

test(makePersonPair2, [fail]) :-
    g1(G),g2(H),makePersonPair(G,H,[]).

test(makePersonPair1, [nondet]) :-
    b1(G),b2(H),makePersonPair(G,H,[p(jeff,amazon),p(bill,microsoft),p(mark,facebook),p(bernard,sephora),p(mukesh,reliance),
                                    p(steve,clippers),p(warren,berkshire),p(larry,google),p(elon,tesla),p(sergey,alphabet)]).

test(makePersonPair2, [fail]) :-
    b1(G),b2(H),makePersonPair(G,H,[]).

% findPersonName(X,K,Y)
test(findPersonName1, [nondet]) :-
    findPersonName(kara,[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],batman).

test(findPersonName2, [fail]) :-
    findPersonName(kara,[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],superman).

test(findPersonName3, [nondet]) :-
    findPersonName(bill,[p(jeff,amazon),p(bill,microsoft),p(mark,facebook),p(bernard,sephora)],microsoft).

test(findPersonName4, [fail]) :-
    findPersonName(jack,[p(jeff,amazon),p(bill,microsoft),p(mark,facebook),p(bernard,sephora)],baidu).

% findNewFollower(XF,K,L)
test(findNewFollower1, [nondet]) :-
    findNewFollower([],[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],[]).

test(findNewFollower2, [nondet]) :-
    findNewFollower([kara],[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],[batman]).

test(findNewFollower3, [nondet]) :-
    findNewFollower([kara, bruce, barry],[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],[batman,green_arrow,supergirl]).

test(findNewFollower4, [nondet]) :-
    findNewFollower([oliver, clark],[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],[superman,flash]).

test(findNewFollower5, [fail]) :-
    findNewFollower([a],[p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],_Y).

% replaceGraph (H,K,F)
test(replaceGraph1, [nondet]) :-
    g1(G), replaceGraph(G, [p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],
            [person(batman, [supergirl, flash]), person(green_arrow,[flash, superman]), person(supergirl, [batman, superman]), 
            person(flash, [superman, batman]), person(superman, [batman])]).

test(replaceGraph2, [nondet]) :-
    g1(G), replaceGraph(G, [p(kara, supergirl), p(bruce, green_arrow), p(barry, batman), p(clark, superman), p(oliver, flash)],
            [person(supergirl, [batman, superman]), person(green_arrow,[superman, flash]), person(batman, [supergirl, flash]),
             person(superman, [flash, supergirl]), person(flash, [supergirl])]).

test(replaceGraph3, [fail]) :-
    g1(G), replaceGraph(G, [p(kara, batman), p(bruce, green_arrow), p(barry, supergirl), p(clark, flash), p(oliver, superman)],
            [person(supergirl, [supergirl, flash]), person(green_arrow,[flash, superman]), person(supergirl, [supergirl, superman]), 
            person(flash, [superman, supergirl]), person(superman, [supergirl])]).

test(replaceGraph4, [nondet]) :-
    b1(G), replaceGraph(G, [p(jeff,amazon),p(bill,microsoft),p(mark,facebook),p(bernard,sephora),p(mukesh,reliance),
                                    p(steve,clippers),p(warren,berkshire),p(larry,google),p(elon,tesla),p(sergey,alphabet)],
            [person(amazon,[microsoft,facebook]),person(microsoft,[clippers,amazon]),person(facebook,[amazon,microsoft,tesla,google,alphabet]),
            person(sephora,[microsoft,amazon]),person(reliance,[]),person(clippers,[microsoft]),person(berkshire,[reliance,sephora]),
            person(google,[alphabet]), person(tesla,[amazon]),person(alphabet,[google])]).

test(replaceGraph5, [fail]) :-
    b1(G), replaceGraph(G, [p(jeff,amazon),p(bill,microsoft),p(mark,facebook),p(bernard,sephora),p(mukesh,reliance),
                            p(steve,clippers),p(warren,berkshire),p(larry,google),p(elon,tesla),p(sergey,alphabet)],[]).

% compareGraph(H, F)
test(compareGraph1, [nondet]) :-
    g1(G), compareGraph(G, G).

test(compareGraph2, [nondet]) :-
    g2(H), compareGraph(H, H).

test(compareGraph3, [fail]) :-
    g1(G), g2(H), compareGraph(G, H).

test(compareGraph4, [nondet]) :-
    b2(H), compareGraph(H, H).

test(compareGraph5, [fail]) :-
    b1(G), b2(H), compareGraph(G, H).

% same_world(H, F, K)
test(same_world1, [nondet]) :-
    same_world([], [], []).

test(same_world2, [nondet]) :-
     g1(G), g2(H), same_world(G, H, [p(kara,supergirl),p(bruce,batman),p(barry,flash),p(clark,superman),p(oliver,green_arrow)]).

test(same_world3, [fail]) :-
    g1(G), b2(H), same_world(G, H, [p(kara,microsoft),p(bruce,facebook),p(barry,sephora),p(clark,reliance),p(oliver,alphabet)]).

:- end_tests(instahub).