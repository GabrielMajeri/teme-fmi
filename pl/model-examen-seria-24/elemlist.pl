/* Predicat ajutător pentru a elimina listele dintr-o listă. */
/* Cazul de bază */
elimina_liste_din_lista([], []).
elimina_liste_din_lista([Head|Tail], Output) :-
    PrimulEsteLista = is_list(Head),
    (
        (
            /* Dacă este listă, elimin */
            PrimulEsteLista,
            Output = SubOutput
        );
        (
            /* Dacă nu e listă, apare în rezultat */
            not(PrimulEsteLista),
            Output = [Head|SubOutput]
        )
    ),
    /* Apel recursiv */
    elimina_liste_din_lista(Tail, SubOutput).

/*
 * Elimină din lista de liste primită ca parametru,
 * toate listele conținute în liste.
 */

/* Cazul de bază */
elimelemlist([], []).
elimelemlist([Head|Tail], Output) :-
    /*
     * Elimin orice liste conține primul element,
     * și apoi îl inserez în rezultat.
     */
    elimina_liste_din_lista(Head, HeadFiltrat),
    Output = [HeadFiltrat|SubOutput],
    /* Apel recursiv */
    elimelemlist(Tail, SubOutput).


/*
 * Predicat ajutător care este `true` dacă
 * lista primită ca parametru nu conține alte liste.
 */
/* Cazul de bază: Lista vidă sigur nu conține alte liste. */
lista_fara_liste([]).
lista_fara_liste([Head|Tail]) :-
    /* Verific să nu fie listă primul element */
    not(is_list(Head)),
    /* Verific recursiv restul */
    lista_fara_liste(Tail).


/* Fiind date două liste, `Output` este lista care are lungime mai mare. */
lista_lungime_maxima(Lista1, Lista2, Output) :-
    length(Lista1, Lungime1),
    length(Lista2, Lungime2),
    (
        (
            Lungime1 >= Lungime2,
            Output = Lista1
        );
        (
            Lungime1 < Lungime2,
            Output = Lista2
        )
    ).

/*
 * Predicat care găsește lista de lungime maximă,
 * care nu conține la rândul ei alte liste.
 */
listmaxfelemlist([Head|Tail], Output) :-
    /* Verific dacă primul element este o posibilitate */
    PrimulNuContineListe = lista_fara_liste(Head),
    (
        (
            PrimulNuContineListe,
            /* Vedem care e cea mai mare listă din ce rămâne */
            AvemAlteElemente = listmaxfelemlist(Tail, SubOutput),
            (
                (
                    AvemAlteElemente,
                    /* Alegem maximul */
                    lista_lungime_maxima(Head, SubOutput, Output)
                );
                (
                    /*
                    * În cazul în care restul listei nu mai are
                    * alte liste cu care să comparăm,
                    * returnăm elementul curent
                    */
                    not(AvemAlteElemente),
                    Output = Head
                )
            )
        );
        (
            not(PrimulNuContineListe),
            /* Caut recursiv maximul */
            listmaxfelemlist(Tail, Output)
        )
    ).
