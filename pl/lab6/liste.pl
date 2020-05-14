/* Predicat care determină numărul de apariții dintr-o listă. */
num_aparitii([], _, 0).

num_aparitii([Head|Tail], Element, Result) :-
    /* Număr de câte ori apare în sublistă */
    num_aparitii(Tail, Element, SubResult),
    (
        (
            Head = Element,
            Result is SubResult + 1
        );
        (
            Head \= Element,
            Result is SubResult
        )
    ).


/* Cifrele unui număr, sub formă de listă */
lista_cifre(Nr, Cifre) :-
    (
        /* Cazul de bază */
        (
            /* Numărul mai are o singură cifră */
            Nr =< 9,
            /* Nu mai concatenez alte cifre la început */
            CelelalteCifre = []
        );
        /* Cazul recursiv */
        (
            /* Mai mult de o cifră rămasă */
            Nr > 9,
            /* Elimin ultima cifră */
            NrFaraUltimaCifra is (Nr div 10),
            /* Găsesc celelalte cifre prin recursivitate */
            lista_cifre(NrFaraUltimaCifra, CelelalteCifre)
        )
    ),

    /* Extrag ultima cifră */
    UltimaCifra is (Nr mod 10),

    /* Reunesc rezultatul într-o listă */
    append(CelelalteCifre, [UltimaCifra], Cifre).


/* Permută circular la stânga lista dată ca parametru */
permuta_circular([Head|Tail], Perm) :- append(Tail, [Head], Perm).

/* Dacă N = 0 nu mai generăm nicio permutare */
list_perm_circular(_, [], 0).
/* Generează N permutări circulare */
list_perm_circular(List, Result, N) :-
    N > 0,
    PrevN is N - 1,
    permuta_circular(List, ListPermutata),
    list_perm_circular(ListPermutata, SubResult, PrevN),
    append([ListPermutata], SubResult, Result).

/* Generează toate permutările circulare ale unei liste */
listpermcirc(List, Result) :-
    length(List, Len),
    list_perm_circular(List, Result, Len).


/* Dacă lista e deja vidă, nu mai trebuie să facem nimic */
elimina([], _, []).

/* Dacă nu e vidă, verificăm dacă primul element trebuie sărit,
 * și continuăm să eliminăm recursiv.
 */
elimina([Head|Tail], Element, Result) :-
    elimina(Tail, Element, SubResult),
    (
        (
            Head = Element,
            Result = SubResult
        );
        (
            Head \= Element,
            Result = [Head|SubResult]
        )
    ).


/* O listă vidă nu are duplicate */
multime([], []).

/* Iau capul listei */
multime([Head|Tail], Multime) :-
    /* îl șterg din restul listei */
    elimina(Tail, Head, TailWithoutElem),
    /* transform recursiv ce rămâne în mulțime */
    multime(TailWithoutElem, SubResult),
    /* pun la loc */
    Multime = [Head | SubResult].


/* Verifică dacă o listă e mulțime */
emult(Lista) :- multime(Lista, Lista).


/* Intersecția a două mulțimi */
inters([], _, []).
inters([Head|Tail], Multime, Result) :-
    inters(Tail, Multime, SubResult),
    (
        (
            member(Head, Multime),
            Result = [Head | SubResult]
        );
        (
            \+ member(Head, Multime),
            Result = SubResult
        )
    ).


/* Diferența a două mulțimi */
diff([], _, []).
diff([Head|Tail], Multime, Result) :-
    diff(Tail, Multime, SubResult),
    (
        (
            member(Head, Multime),
            Result = SubResult
        );
        (
            \+ member(Head, Multime),
            Result = [Head | SubResult]
        )
    ).


/* Produsul dintre un element și o mulțime */
prod_elem_multime(_, [], []).
prod_elem_multime(Elem, [Head|Tail], Result) :-
    prod_elem_multime(Elem, Tail, SubResult),
    append([(Elem, Head)], SubResult, Result).

/* Produsul cartezian */
prod_cartezian([], _, []).
prod_cartezian([Head|Tail], Multime, Result) :-
    /* Combin primul element cu toată cealaltă mulțime */
    prod_elem_multime(Head, Multime, SubProd),
    prod_cartezian(Tail, Multime, SubResult),
    append(SubProd, SubResult, Result).
