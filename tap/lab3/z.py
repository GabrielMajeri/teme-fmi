"""
Parcurgere în Z

Pe o tablă de latură 2^n avem numerele de la 1 la 2^n * 2^n.

Pentru n = 1:
1 2
3 4

Pentru n = 2:
 1  2   5  6
 3  4   7  8
 9 10  13 14
11 12  15 16

Soluție ineficientă: construim și memorăm tabla, O(2^n)
și multă memorie consumată.

Interogările: sunt de forma (i, j) - ce număr apare pe
linia i, coloana j.

Complexitate: din teorema master



"""

fin = open('z.in')

n, k = map(int, next(fin).split())

BASE_CASE = [[1, 2],
             [3, 4]]


def find(dim, x, y):
    # print(f'({x}, {y}) in {dim}')

    if dim == 1:
        return BASE_CASE[x - 1][y - 1]

    half = dim // 2
    num = dim ** 2
    if x <= half:
        if y <= half:
            # Cadranul I
            return find(half, x, y)
        else:
            # Cadranul II
            return num // 4 + find(half, x, y - half)
    else:
        if y <= half:
            # Cadranul III
            return num // 2 + find(half, x - half, y)
        else:
            # Cadranul IV
            return 3 * num // 4 + find(half, x - half, y - half)


dim = 2 ** n
with open('z.out', 'w') as fout:
    for _ in range(k):
        x, y = map(int, next(fin).split())
        print(find(dim, x, y), file=fout)
