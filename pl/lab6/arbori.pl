/*** Arbori binari ***/

/* Parcurgeri */

srd(nil, []).
srd(arb(Radacina, ArbStang, ArbDrept), Output) :-
    /* Apelez recursiv parcurgerea */
    srd(ArbStang, ListaStanga),
    srd(ArbDrept, ListaDreapta),
    /* Construiesc rezultatul */
    append(ListaStanga, [Radacina], TempOutput),
    append(TempOutput, ListaDreapta, Output).

rsd(nil, []).
rsd(arb(Radacina, ArbStang, ArbDrept), Output) :-
    rsd(ArbStang, ListaStanga),
    rsd(ArbDrept, ListaDreapta),
    append([Radacina], ListaStanga, TempOutput),
    append(TempOutput, ListaDreapta, Output).

sdr(nil, []).
sdr(arb(Radacina, ArbStang, ArbDrept), Output) :-
    sdr(ArbStang, ListaStanga),
    sdr(ArbDrept, ListaDreapta),
    append(ListaStanga, ListaDreapta, TempOutput),
    append(TempOutput, [Radacina], Output).


/* Determină frunzele unui arbore binar */
frunze(nil, []).
frunze(arb(Radacina, nil, nil), [Radacina]).
frunze(arb(_, ArbStang, ArbDrept), Output) :-
    frunze(ArbStang, FrunzeStanga),
    frunze(ArbDrept, FrunzeDreapta),
    append(FrunzeStanga, FrunzeDreapta, Output).
