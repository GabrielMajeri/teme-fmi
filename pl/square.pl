/* Când se termină linia punem un caracter de rând nou. */
line(0, _) :- nl.
/* Scriem un rând de N caracter, folosind recursia. */
line(N, Char) :- N > 0, write(Char), Pred is N - 1, line(Pred, Char).

/* Cazul de bază pentru pătrat, când nu avem de desenat nimic. */
square1(_, 0, _).
/* Cazul recursiv, scriem o linie și continuăm să scriem N - 1 linii. */
square1(NOrig, N, Char) :- N > 0, line(NOrig, Char),
    Pred is N - 1,
    square1(NOrig, Pred, Char).

/* Cerința vroia un predicat cu doi parametrii. */
square(N, Char) :- square1(N, N, Char).
