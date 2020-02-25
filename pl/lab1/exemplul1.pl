/* Toți membrii familiei Stark.
 * Dacă dau interogarea `lannister(X)` o să-mi afișeze toți membrii.
 */
stark(eddard).
stark(jon_snow).
stark(sansa).

/* Toți membrii familiei Lannister. */
lannister(tyrion).
lannister(cersei).

/* Cersei nu-l place pe Tyrion. */
dislike(cersei, tyrion).
