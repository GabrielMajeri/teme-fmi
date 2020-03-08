/* Trebuie să reprezentăm consiliul lui Joffrey,
 * indicând cine stă la dreapta cui la masă.
 */

sits_right_of("Petyr Baelish", "Varys").
sits_right_of("Tywin Lannister", "Petyr Baelish").
sits_right_of("Cersei Baratheon", "Tywin Lannister").
sits_right_of("Janos Slynt", "Cersei Baratheon").
sits_right_of("Tyrion Lannister", "Janos Slynt").
sits_right_of("Grand Maester Pycelle", "Tyrion Lannister").
sits_right_of("Varys", "Grand Maester Pycelle").

/* Verifică dacă X e la stânga lui Y. */
sits_left_of(X, Y) :- sits_right_of(Y, X).

/* Verifică dacă X e la stânga lui Z și Y e la dreapta lui Z la masă.
 * Exemplu de interogare:
 *   pentru a vedea cine stă lângă Varys, scriu
 *     are_neighbors_of(X, Y, "Varys").
 */
are_neighbors_of(X, Y, Z) :- sits_left_of(X, Z), sits_right_of(Y, Z).

/* Verifică dacă X e la stânga sau dreapta lui Y.
 * De exemplu, `next_to_each_other("Varys", "Tyrion Lannister").` o să fie false.
 */
next_to_each_other(X, Y) :- sits_left_of(X, Y); sits_right_of(X, Y).

/* Interogări de rulat: */

/* Este Petyr Baelish la dreapta lui Cersei Baratheon?
 *   sits_right_of("Petyr Baelish", "Cersei Baratheon").
 */

/* Este Petyr Baelish la dreapta lui Varys?
 *   sits_right_of("Petyr Baelish", "Varys").
 */

/* Cine este la dreapta lui Janos Slynt?
 *   sits_right_of(X, "Janos Slynt").
 */

/* Cine stă doua scaune la dreapta lui Cersei Baratheon?
 *   sits_right_of(Y, X), sits_right_of(X, "Cersei Baratheon").
 *     Y = "Tyrion Lannister",
 *     X = "Janos Slynt" .
 */

/* Cine stă între Petyr Baelish și Grand Maester Pycelle?
 *   next_to_each_other(X, "Petyr Baelish"), next_to_each_other(X, "Grand Maester Pycelle").
 *
 * Am folosit `next_to_each_other` pentru că, în comparație cu `are_neighbors_of`, acesta nu depinde de ordine
 * (adică nu contează dacă zic „între Baelish și Pycelle” sau „între Pycelle și Baelish”).
 */
