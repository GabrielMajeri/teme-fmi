/* Cazul de bază */
persoane([], _, []).

/* Dacă persoana are obiectul */
persoane([are(Persoana, Obiecte)|Tail], Obiect, [Persoana|Persoane]) :-
    member(Obiect, Obiecte),
    persoane(Tail, Obiect, Persoane).

/* Dacă persoana nu are obiectul */
persoane([are(_, Obiecte)|Tail], Obiect, Persoane) :-
    not(member(Obiect, Obiecte)),
    persoane(Tail, Obiect, Persoane).
