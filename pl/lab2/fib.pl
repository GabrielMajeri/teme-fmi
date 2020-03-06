/* Cazurile de bază: */
/* Pentru N = 0, F_0 = 1, predecesorul este 0 */
fib(0, 1, 0).
/* Pentru N = 1, F_1 = 1, predecesorul este 1 */
fib(1, 1, 1).

/* Predicat pentru calcularea al N-ulea număr Fibonacci.
 *
 * Aceasta este soluția mai eficientă, care reține ultima valoare Fibonacci
 * pe măsură ce le calculează.
 */
fib(N, X, Y) :-
    /* Această formulă recurentă e folosită doar pentru N >= 2 */
    N >= 2,

    /* Folosim `is` pentru a unifica (calcula) N - 1 */
    Pred is N - 1,

    /* Calculăm Fibonacci pentru N - 1, și dăm ca parametru valoarea precedentă */
    fib(Pred, Y, Z),

    /* Punem rezultatul în X */
    X is Y + Z.

/* Cerința vroia un predicat de aritate 2 */
fib(N, X) :- fib(N, X, _).
