/* Lista vidă este deja sortată */
quicksort([], []).

/* Lista cu un singur element este deja sortată */
quicksort([Elem], [Elem]).

/* Folosesc primul element ca pivot */
quicksort([Pivot|Rest], Sorted) :-
    /* Partiționez lista */
    partition(>(Pivot), Rest, Smaller, Bigger),
    /* Sortez numerele mai mici */
    quicksort(Smaller, SortedSmaller),
    /* Sortez numerele mai mari */
    quicksort(Bigger, SortedBigger),
    /* Reunesc rezultatul */
    append(SortedSmaller, [Pivot], HalfSorted),
    append(HalfSorted, SortedBigger, Sorted).
