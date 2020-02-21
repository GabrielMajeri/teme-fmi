/* If the list has only one element, then that element is the last one. */
last_element(X, [X]).

/* If the list has more elements, then we skip the first element and
 * keep looking for the last one.
 */
last_element(X, [Head|Tail]) :- last_element(X, Tail).
