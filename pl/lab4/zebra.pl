/* `X` este succesorul lui `Y` */
la_dreapta(X, Y) :- X is Y + 1.

/* `Y` este succesorul lui `X` */
la_stanga(X, Y) :- la_dreapta(Y, X).

/* Lângă înseamnă la dreapta sau la stânga */
langa(X, Y) :- la_dreapta(X, Y); la_stanga(X, Y).

solutie(Strada, PosesorZebra) :-
    Strada = [
        /* casa(Numar, Nationalitate, Culoare, Animal, Băutură, Țigări) */
        casa(1, norvegian, _, _, _, _),
        casa(2, _, _, _, _, _),
        casa(3, _, _, _, lapte, _),
        casa(4, _, _, _, _, _),
        casa(5, _, _, _, _, _)
    ],

    member(casa(_, englez, rosie, _, _, _), Strada),
    member(casa(_, spaniol, _, caine, _, _), Strada),
    member(casa(CasaVerde, _, verde, _, cafea, _), Strada),
    member(casa(_, ucrainean, _, _, ceai, _), Strada),
    member(casa(CasaBej, _, bej, _, _, _), Strada),
    la_dreapta(CasaVerde, CasaBej),
    member(casa(_, _, _, melci, _, oldGold), Strada),
    member(casa(CasaKools, _, galben, _, _, kools), Strada),
    member(casa(CasaChesterfields, _, _, _, _, chesterfields), Strada),
    member(casa(CasaVulpe, _, _, vulpe, _, _), Strada),
    langa(CasaChesterfields, CasaVulpe),
    member(casa(CasaCal, _, _, cal, _, _), Strada),
    langa(CasaKools, CasaCal),
    member(casa(_, _, _, _, suc, luckyStrike), Strada),
    member(casa(_, japonez, _, _, _, parliaments), Strada),
    member(casa(CasaAlbastra, _, albastru, _, _, _), Strada),
    langa(1, CasaAlbastra),
    member(casa(PosesorZebra, _, _, zebra, _, _), Strada).
