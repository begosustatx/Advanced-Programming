% Advanced Programming Assignment 3
% Skeleton for warm-up part. Predicates to implement:

% add(N1, N2, N)
add(z, N2, N2).
add(s(N1), N2, N) :-  add(N1, s(N2), N).

% mult(N1, N2, N)
mult(z, _, z).
mult(s(N1), N2, N) :- mult(N1, N2, N3), add(N2, N3, N).

% comp(N1, N2, A)
comp(z, z, eq).
comp(s(_), z, gt).
comp(z, s(_), lt).
comp(s(N1), s(N2), A) :- comp(N1, N2, A).

% insert(N, TI, TO)
insert(N, leaf, node(N, leaf, leaf)).
insert(N1, node(N2, T1, T2), node(N2, T, T2)) :- comp(N1, N2, lt), insert(N1, T1, T).
insert(N1, node(N2, T1, T2), node(N2, T1, T)) :- comp(N1, N2, gt), insert(N1, T2, T).
insert(N1, node(N2, T1, T2), node(N2, T1, T2)) :- comp(N1, N2, eq).

% insertlist(Ns, TI, TO)
insertlist([], T, T).
insertlist([N|Ns], T1, T) :- insert(N, T1, T2), insertlist(Ns, T2, T).