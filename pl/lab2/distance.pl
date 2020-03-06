/* Calculează distanța Euclidiană dintre două puncte. */
distance((X1, Y1), (X2, Y2), X) :- X is sqrt((X2 - X1) ** 2 + (Y2 - Y1) ** 2).
