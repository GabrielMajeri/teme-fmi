/* Cazul de bază al recursivității */
suma_si_nr_aparitii([], _, 0, 0).
suma_si_nr_aparitii([sn(Nume, Nota)|T], Student, Suma, NrAp) :-
    /* Apelez recursiv pe lista rămasă */
    suma_si_nr_aparitii(T, Student, SubSuma, SubNrAp),
    (
        (
            Nume = Student,
            Suma is SubSuma + Nota,
            NrAp is SubNrAp + 1
        );
        (
            Nume \= Student,
            Suma is SubSuma,
            NrAp is SubNrAp
        )
    ).

/* Calculează media notelor unui student */
media(ListaStudenti, Student, Media) :-
    suma_si_nr_aparitii(ListaStudenti, Student, Suma, NrAp),
    Media is Suma / NrAp.


cuvant(testul, 6).
cuvant(usor, 4).
cuvant(este, 4).
cuvant(ada, 3).
cuvant(carte, 5).

/* Multimea vidă este singura posibilitate pentru lungime 0 */
exista(0, []).
exista(Nr, MultimeCuvinte) :-
    cuvant(Cuvant, Lungime),
    atom_length(Cuvant, Lungime),
    (
        /* Cazul de bază: avem un cuvânt de lungime potrivită */
        (
            Lungime = Nr,
            MultimeCuvinte = [Cuvant]
        );
        /* Cazul recursiv: aleg un cuvânt mai scurt decât limita, și încerc
        * să-i găsesc o potrivire.
        */
        (
            Lungime < Nr,
            SpatiuLiber is Nr - Lungime,
            exista(SpatiuLiber, SubMultime),
            /* Nu am voie să repet cuvintele */
            \+ member(Cuvant, SubMultime),
            append([Cuvant], SubMultime, MultimeCuvinte)
        )
    ).



/* Declar variabilele */
isvar(X) :- member(X, [a, b, c, d, e, f]).

/* Simplific adunarea cu zero */
simplifica(plus(zero, B), SimplB) :- simplifica(B, SimplB).
simplifica(plus(A, zero), SimplA) :- simplifica(A, SimplA).
/* Simplific recursiv adunarea */
simplifica(plus(A, B), plus(SimplA, SimplB)) :-
    simplifica(A, SimplA), simplifica(B, SimplB).

/* Simplific înmulțirea cu zero */
simplifica(ori(_, zero), zero).
simplifica(ori(zero, _), zero).
/* Simplific recursiv înmulțirea */
simplifica(ori(A, B), ori(SimplA, SimplB)) :-
    simplifica(A, SimplA), simplifica(B, SimplB).

/* Cazul de bază */
simplifica(Expresie, Expresie).


/* Cazul de bază */
valoare_variabila([vi(Variabila, Valoare)], Variabila, Valoare).
/* Cazul recursiv */
valoare_variabila([vi(Nume, Valoare)|T], Variabila, Rezultat) :-
    (
        Nume = Variabila,
        Rezultat = Valoare
    );
    (
        Nume \= Variabila,
        valoare_variabila(T, Variabila, Rezultat)
    ).

/* Valoarea unei variabile se poate găsi în memorie */
valoare(Memorie, Variabila, Rezultat) :-
    isvar(Variabila),
    valoare_variabila(Memorie, Variabila, Rezultat).

/* Pentru expresiile compuse, calculăm recursiv valoarea fiecărei părți
 * și reunim rezultatul.
 */
valoare(Memorie, plus(A, B), Rezultat) :-
    valoare(Memorie, A, ValoareA),
    valoare(Memorie, B, ValoareB),
    Rezultat is ValoareA + ValoareB.

valoare(Memorie, ori(A, B), Rezultat) :-
    valoare(Memorie, A, ValoareA),
    valoare(Memorie, B, ValoareB),
    Rezultat is ValoareA * ValoareB.
