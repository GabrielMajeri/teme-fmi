"""
Se dă o tablă de dimensiune 2^N * 2^N completată după următoarea regulă:

 1  3  9 11
 4  2 12 10
13 15  5  7
16 14  8  6

Fiind dat un număr k, trebuie determinat rândul și coloana pe care se află
în matrice.
"""

def find(dim, k):
    if dim == 2:
        if k == 1:
            return (1, 1)
        elif k == 2:
            return (2, 2)
        elif k == 3:
            return (1, 2)
        else:
            return (2, 1)

    half = dim // 2
    num = dim ** 2

    quarter = num // 4

    if k <= quarter: # Stânga-sus
        i, j = find(half, k)
        return i, j
    elif k <= 2 * quarter: # Dreapta-jos
        i, j = find(half, k - quarter)
        return i + half, j + half
    elif k <= 3 * quarter: # Dreapta-sus
        i, j = find(half, k - 2 * quarter)
        return i, j + half
    else: # Stânga-jos
        i, j = find(half, k - 3 * quarter)
        return i + half, j


with open("tabla.in") as fin:
    line = next(fin)
    n, k = map(int, line.split())

dim = 2 ** n
i, j = find(dim, k)

print(i, j)
