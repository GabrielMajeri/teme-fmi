/* Fapte */
q(a).
q(b).

r(c).
r(d).

s(e).

/* Predicate */
top(X, Y) :- p(X, Y).
top(X, X) :- s(X).

p(X, Y) :- q(X), r(Y).
p(X, Y) :- s(X), r(Y).

/* ?- top(X, Y). */
