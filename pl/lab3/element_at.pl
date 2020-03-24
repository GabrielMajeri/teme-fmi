/* Cazul de bază, când vrem primul element. */
element_at([H|_], 1, X) :- X = H.

element_at([_|T], Num, X) :-
    /* Apelăm `element_at` recursiv, pe o list mai mică. */
    Pred is Num - 1,
    element_at(T, Pred, X).
