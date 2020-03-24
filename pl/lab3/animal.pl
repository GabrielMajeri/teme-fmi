/* Animal database */

animal(alligator).
animal(tortue).
animal(caribou).
animal(ours).
animal(cheval).
animal(vache).
animal(lapin).

/* Încearcă să formeze un mutant din listele `X` și `Y` */
mutante(X, Y, Mutant) :-
    /* Generez simultan prefixele și sufixele lui `X` */
    append(PrefX, SufX, X), SufX \= [],
    /* Generez simultan prefixele și sufixele lui `Y` */
    append(PrefY, SufY, Y), PrefY \= [],
    /* Trebuie să se potrivească prefixul cu sufixul */
    SufX = PrefY,
    /* Combinăm partea distinctă din cuvinte */
    append(PrefX, SufY, Mutant).

mutant(Mutant) :-
    /* Luăm două animale */
    animal(X), animal(Y),
    /* Mutația trebuie să fie între două animale diferite */
    X \= Y,
    /* Transform atomii în liste de caractere */
    name(X, LitereX), name(Y, LitereY),
    /* Vedem dacă pot fi mutante */
    mutante(LitereX, LitereY, LitereMutant),
    /* Transform înapoi în atom */
    name(Mutant, LitereMutant).
