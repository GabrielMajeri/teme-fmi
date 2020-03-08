/* Definim predicatul `char` care indică că atomul respectiv
 * este un personaj din povestea noastră.
 */
char(king).
char(priest).
char(richMan).

/* Definim predicatul `not_char` care este folosit pentru a selecta
 * celelalte personaje în afară de personajul ales.
 */
not_char(C, X) :- char(X), X \= C.


/* Fiecărei alegeri îi corespunde un personaj care nu va fi ucis. */
choice(god, priest).
choice(authority, king).
choice(money, richMan).


/* Predicatul `is_killed` determină mai întâi ce personaj nu va fi ucis,
 * apoi returnează celelalte două personaje care vor muri.
 *
 * Notă: acest predicat va returna și (`king`, `richMan`) dar și (`richMan`, `king`).
 */
is_killed(C, X, Y) :- choice(C, NotKilled),
    not_char(NotKilled, X), not_char(NotKilled, Y),
    X \= Y.
