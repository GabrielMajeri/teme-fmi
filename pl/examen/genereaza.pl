/* Predicat care adaugă Cifra la capătul fiecărui număr din listă */
concateneaza(_, [], []).
concateneaza(Cifra, [Head1|Tail1], [Head2|Tail2]) :-
    Head2 is Head1 * 10 + Cifra,
    concateneaza(Cifra, Tail1, Tail2).

genereaza(0, []).
genereaza(1, [1, 2, 3]).

genereaza(N, ListaNumere) :-
    N > 1,
    Pred is N - 1,
    genereaza(Pred, ListaPred),
    concateneaza(1, ListaPred, Nr1),
    concateneaza(2, ListaPred, Nr2),
    concateneaza(3, ListaPred, Nr3),
    append([Nr1, Nr2, Nr3], ListaNumere).
