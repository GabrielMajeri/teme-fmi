"""
Se dă un vector `v` de `n` numere întregi.

Fie k, l naturale astfel încât k * l <= n.

Să se aleagă din vectorul `v` un număr de `k` secvențe (poziții consecutive)
de lungime `l` a. î. suma elementelor alese să fie maximă.

În primul rând, construim vectorul de sume parțiale.

Cazul k = 1:
    Reținem un vector M[i] = max {
            S[i] - S[i - l] dacă i apare în secvență
            M[i - 1] dacă i nu apare în secvență
        }

Cazul k = 2:
    Construiesc M, apoi mai construiesc și pe N:

    N[i] = max {
            (S[i] - S[i - l]) + M[i - l] dacă i este ales în a doua secvență
            N[i - 1] dacă i nu este ales
        }

Exemplu:
    v = -2 5  7  3 10  4 -2  8 -5
    S = -2 3 10 13 23 27 25 33 28

    // pentru k = 1
    M = 0 3 12 12 13 14 14 14 14

    // pentru k = 2
    N = 0 0 0 13 25 26 26 26 26

    // pentru k = 3:
    M = N de la k = 2
    N = 0 0 0 0 0 27 27 32 32 32
"""

with open("sumemax.txt") as fin:
    v = [int(x) for x in next(fin).split()]


print(v)
