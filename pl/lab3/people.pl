/* Oameni cu zilele de naștere corespunzătoare. */

born(jan, date(20,3,1977)).
born(jeroen, date(2,2,1992)).
born(joris, date(17,3,1995)).
born(jelle, date(1,1,2004)).
born(joan, date(24,12,0)).
born(joop, date(30,4,1989)).
born(jannecke, date(17,3,1993)).
born(jaap, date(16,11,1995)).


/* Căutăm persoane născute în orice zi/lună, dar în acel an. */
year(Year, Person) :- born(Person, date(_, _, Year)).

before(date(D1, M1, Y1), date(D2, M2, Y2)) :-
    /* Dacă este înainte cu câțiva ani */
    Y1 < Y2;
    /* Dacă este în același an dar cu câteva luni mai devreme */
    Y1 = Y2, M1 < M2;
    /* Dacă este în aceeași an și lună, dar cu câteva zile mai devreme */
    Y1 = Y2, M1 = M2, D1 < D2.

older(Person, Other) :-
    born(Person, Date1), born(Other, Date2),
    /* Trebuie ca persoana mai în vârstă să se fi născut înaintea
     * celei mai tinere.
     */
    before(Date1, Date2).
