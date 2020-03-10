/* O listă vidă este acceptată. */
all_a([]).

/* Verificăm că primul element din listă este `a`, și apoi că restul listei
 * conține doar `a`-uri.
 */
all_a([a|T]) :- all_a(T).

/* Cazul de bază, când ambele liste sunt vide. */
trans_a_b([], []).
/* Cazul recursiv, verific ca primele elemente ale ambelor liste să fie
 * `a` respectiv `b`, și apoi apeleze recursiv predicatul. */
trans_a_b([H1|T1], [H2|T2]) :-
    H1 = a, H2 = b,
    trans_a_b(T1, T2).

/* Când lista este vidă, nu am ce să înmulțesc. */
scalarMult(_, [], []).

/* Descompun lista, înmulțesc primul element cu scalarul,
 * și folosesc recursia pe restul listei.
 */
scalarMult(Scalar, [H|T], Result) :-
    X is Scalar * H,
    scalarMult(Scalar, T, SubResult),
    Result = [X|SubResult].

/* Presupun că produsul scalar a doi vectori vizi este 0. */
dot([], [], 0).
/* Calculez recursiv produsul scalar. */
dot([H1|T1], [H2|T2], Result) :-
    Prod is H1 * H2,
    dot(T1, T2, SubResult),
    Result is Prod + SubResult.

/* Dacă primesc o listă vidă o să atribui -1 rezultatului. */
max([], -1).
/* Compar capul listei cu maximul din ce e în dreapta. */
max([H|T], Result) :-
    max(T, SubMax),
    /* Dacă H este mai mare, se va folosi clauza din stânga, și
     * rezultatul va fi H. Altfel, se va folosi clauza din dreapta.
     */
    ((H > SubMax, Result is H); (H =< SubMax, Result is SubMax)).


/* Trebuie să folosesc o implementare proprie de `reverse`: */
myReverse([], []).
myReverse([H|T], Reverse) :-
    /* Aplic recursiv pe sublista rămasă. */
    myReverse(T, SubReverse),
    /* Iau inversa a ce a rămas, adaug primul element. */
    append(SubReverse, [H], Reverse).

/* Verific dacă lista este egală cu inversa ei. */
palindrome(List) :-
    myReverse(List, Reverse),
    List = Reverse.


/* Parcurg lista, rețin într-o variabilă auxiliar */
remdup([], Seen, Result) :- Result = Seen.

remdup([H|T], Seen, Result) :-
    /* Dacă H deja a fost văzut, atunci nu îl adăugăm la rezultat,
     * doar continuăm parcurgerea listei.
     */
    ((member(H, Seen), remdup(T, Seen, Result));
    /* Dacă H nu a mai fost văzut, îl punem în lista Seen,
     * și continuăm parcurgerea.
     */
    (not(member(H, Seen)), append([H], Seen, NewSeen), remdup(T, NewSeen, Result))).

remove_duplicates(List, Result) :-
    remdup(List, [], Result).
