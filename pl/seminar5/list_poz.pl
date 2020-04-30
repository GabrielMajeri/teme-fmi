/* Pentru lista vidă nu fac nimic */
list_poz1([], [], _).

/* Iau primul element */
list_poz1([Elem|Tail], Result, Offset) :-
    /* Continui recursiv prin listă */
    NextOffset is Offset + 1,
    list_poz1(Tail, SubResult, NextOffset),
    /* Pun poziția pentru primul element */
    append([(Elem, Offset)], SubResult, Result).

/* Wrapper pentru implementare */
list_poz(List, Result) :-
    list_poz1(List, Result, 1).
