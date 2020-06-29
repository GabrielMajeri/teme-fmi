/* Joc de x si 0 jucator contra jucator.
Putem juca si alte jocuri, eventual cu alta dimensiune a tablei si cu alte simboluri de pus in locatiile vacante de pe tabla. */

:- dynamic config/1.

/* Predicatul "start" incepe jocul: retrage din baza de cunostinte configuratia finala a tablei din jocul anterior
si orice alta configuratie memorata, apoi adauga la baza de cunostinte configuratia initiala a tablei, pe care o
afiseaza pe ecran. O varianta de implementare este:

start :- not(stergeconfig), asserta(config([[v,v,v],[v,v,v],[v,v,v]])), aratatabla.

stergeconfig :- retract(config(_)), stergeconfig.

Conjunctia e evaluata de la stanga la dreapta, deci "stergeconfig" se va reapela pana cand "retract" intoarce false,
adica pana cand nu mai exista configuratii in baza de cunostinte, iar la final va intoarce false, motiv pentru care,
in definitia lui "start", trebuie sa il inseram negat la inceputul acelei conjunctii.
Echivalent, putem folosi predicatul predefinit "retractall", care va avea ca efect stergerea tuturor faptelor din
definitia lui "config" si apoi va intoarce true: */

start :- retractall(config(_)), asserta(config([[v,v,v],[v,v,v],[v,v,v]])), aratatabla.

aratatabla :- config(ListaLinii), afiseaza(ListaLinii).

afiseaza([]) :- nl.
afiseaza([L|LL]) :- scrie(L), nl, afiseaza(LL).

scrie([]).
scrie([v|T]) :- write('.'), scrie(T).   %%% Putem sa punem tab(1) sau write('_') in loc de write('.') pentru locatiile vacante.
scrie([H|T]) :- H\=v, write(H), scrie(T).

pune(Simbol,Linie,Coloana) :-
    (
        /* Verific să fie rândul acestui simbol */
        rand(Simbol),
        retract(config(ListaLinii)),
        modif(Simbol,ListaLinii,Linie,Coloana,NouaListaLinii),
        asserta(config(NouaListaLinii)), aratatabla,
        aratacastigator
    );
    (
        write('nu este randul tau'),
        nl
    ).

% Simbolul va fi x sau 0 in cazul jocului de x si 0.

modif(_,[],_,_,_).   %%% Ajungem aici daca am dat un numar de linie mai mare decat numarul de linii al tablei.
modif(Simbol,[LiniaCurenta|ListaLinii],1,Coloana,[NouaLinieCurenta|ListaLinii]) :- modiflin(Simbol,LiniaCurenta,Coloana,NouaLinieCurenta).
modif(Simbol,[LiniaCurenta|ListaLinii],Linie,Coloana,[LiniaCurenta|NouaListaLinii]) :- Linie>1, L is Linie-1, modif(Simbol,ListaLinii,L,Coloana,NouaListaLinii).

modiflin(_,[],_,_).   %%% Ajungem aici daca am dat un numar de coloana mai mare decat numarul de coloane al tablei.
modiflin(Simbol,[H|T],Coloana,[H|L]) :- Coloana>1, C is Coloana-1, modiflin(Simbol,T,C,L).
modiflin(Simbol,[v|T],1,[Simbol|T]).
modiflin(_,[H|T],1,[H|T]) :- H\=v, write('locatie ocupata'), nl.

/* Jocul se desfasoara in maniera urmatoare:
?- start.
?- pune(x,2,1).
?- pune(0,2,2).
?- pune(x,2,2).
Aici primim eroare: "locatie ocupata", si configuratia tablei nu se schimba.
?- pune(x,3,3).
S. a. m. d..
Desigur, se pot face imbunatatiri: daca pe tabla sunt mai multi x decat 0, sa nu se accepte punerea unui x; dupa fiecare mutare sa se verifice
daca unul dintre jucatori a castigat; daca se umple tabla, jocul sa se declare incheiat, cu sau fara victoria unuia dintre jucatori. */

/* v indică spațiu gol, deci nu îl număr */
numara_simbol(v, 0).
numara_simbol(x, 1).
numara_simbol(0, 1).

/* Numără câte simboluri au fost puse pe o linie. */
numara_linie([A, B, C], Nr) :-
    numara_simbol(A, NrA),
    numara_simbol(B, NrB),
    numara_simbol(C, NrC),
    Nr is NrA + NrB + NrC.

/* Predicat care numără câte simboluri au fost puse deja pe tablă. */
numara(Nr) :- config([L1, L2, L3]),
    numara_linie(L1, Nr1),
    numara_linie(L2, Nr2),
    numara_linie(L3, Nr3),
    /* Adunăm numărul de simboluri de pe fiecare linie */
    Nr is Nr1 + Nr2 + Nr3.

/* Predicat care este adevărat când s-a umplut tabla de simboluri */
tabla_plina :- numara(Nr), Nr = 9.

/* Pentru a vedea a cui e rândul: numărăm câte simboluri sunt,
 * și vedem dacă acest număr este par (e rândul lui X) sau impar (e rândul lui 0)
 */
rest_nr_piese(Rest) :- numara(Nr), Rest is Nr mod 2.
rand(x) :- rest_nr_piese(0).
rand(0) :- rest_nr_piese(1).

/* Toate modurile în care se poate câștiga */
/* Linia 1 */
castiga([[S, S, S], _, _], S).
/* Linia 2 */
castiga([_, [S, S, S], _], S).
/* Linia 3 */
castiga([_, _, [S, S, S]], S).

/* Coloana 1 */
castiga([[S, _, _], [S, _, _], [S, _, _]], S).
/* Coloana 2 */
castiga([[_, S, _], [_, S, _], [_, S, _]], S).
/* Coloana 3 */
castiga([[_, _, S], [_, _, S], [_, _, S]], S).

/* Diagonala principală */
castiga([[S, _, _], [_, S, _], [_, _, S]], S).
/* Diagonala secundară */
castiga([[_, _, S], [_, S, _], [S, _, _]], S).

/* Predicat care e adevărat când avem un câștigător */
castigator(Simbol) :-
    config(Tabla),
    castiga(Tabla, Simbol),
    /* Simbolul tablă goală nu poate să fie câștigător */
    Simbol \= v.

/* E remiză dacă tabla e plină nu a câștigat nimeni */
remiza :- tabla_plina, \+ castigator(_).

aratacastigator :-
    (remiza, write('remiza'), nl);
    (castigator(S), write('a castigat '), write(S), nl);
    true.
