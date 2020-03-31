word_letters(Word, Letters) :-
    atom_chars(Word, Letters).


/* Verifică dacă prima listă este inclusă în a doua listă. */
cover([], _).
cover([H|T], Cover) :-
    member(H, Cover),
    delete(Cover, H, CoverWithoutH),
    cover(T, CoverWithoutH).

/* Găsește o soluție `Word` de lungime `Length` folosind `Letters`. */
solution(Letters, Word, Length) :-
    word(Word),
    word_letters(Word, WordLetters),
    length(WordLetters, Length),
    cover(WordLetters, Letters).

topsolutionMaxLen(Letters, TopWord, TopLength, MaxLength) :-
    /* Caut un cuvânt potrivit de lungime `MaxLength` */
    solution(Letters, TopWord, MaxLength),
    TopLength = MaxLength;
    /* Alternativ: încercăm cuvinte de lungime `MaxLength - 1` */
    PrevMaxLength is MaxLength - 1,
    topsolutionMaxLen(Letters, TopWord, TopLength, PrevMaxLength).

topsolution(Letters, TopWord, TopLength) :-
    length(Letters, MaxLength),
    topsolutionMaxLen(Letters, TopWord, TopLength, MaxLength).
