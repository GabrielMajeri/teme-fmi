/* O listă vidă conține trivial doar `a`-uri. */
all_a([]).

/* Verificăm că primul element din listă este `a`, și apoi că restul listei
 * conține doar `a`-uri.
 */
all_a([a|T]) :- all_a(T).
