/* Succesorul unui număr se obține adăugând un `x` la listă. */
succesor(L, Result) :- append(L, [x], Result).

/* Predecesorul este inversul succesorului. */
predecessor(L, Result) :- succesor(Result, L).

/* Adună două numere concatenând listele. */
plus(A, B, Result) :- append(A, B, Result).

/* Cazul de bază, înmulțirea cu 1 */
times(A, [x], Result) :- Result = A.

times(A, B, Result) :-
    /* Calculez B - 1 */
    predecessor(B, PredB),
    /* Calculez recursiv A * (B - 1) */
    times(A, PredB, Temp),
    /* Adun încă un A */
    plus(Temp, A, Result).
