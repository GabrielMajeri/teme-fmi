/* Definim culorile */
culoare(albastru).
culoare(galben).
culoare(rosu).
culoare(verde).

/* Definim harta:
 * țările sunt variabile, și valoarea lor va fi culoarea
 * Exemplu:
 *   RO = roșu,
 */
harta(RO, SE, MD, UA, BG, HU) :- vecin(RO, SE), vecin(RO, UA),
                                 vecin(RO, MD), vecin(RO, BG),
                                 vecin(RO, HU), vecin(UA, MD),
                                 vecin(BG, SE), vecin(SE, HU).

/* Definesc condiția de vecinătate:
 * nu au voie să se învecineze două țări cu aceeași culoare.
*/
vecin(X, Y) :- culoare(X),
               culoare(Y),
               X \== Y.

/* Pentru a găsi soluția, rulez:
 * harta(RO, SE, MD, UA, BG, HU).
 */
