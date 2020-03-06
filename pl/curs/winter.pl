/* It's windy in Oslo */
windy(oslo).

/* Oslo is norwegian */
norway(oslo).

/* Everything in Norway is cold */
cold(X) :- norway(X).

/* If it's cold and windy then winter is coming! */
winterIsComing(X) :- cold(X), windy(X).
