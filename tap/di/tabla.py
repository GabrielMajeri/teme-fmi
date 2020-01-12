"""
T(4^n) = 4 T(4^n/4) + O(1)

T(k) = 4 T(k / 4) + O(1)

Linear după numărul de casetuțe pe tablă

O(4^n)
"""

fin = open('tabla.txt')

n = int(next(fin))
missing = tuple(int(x) - 1 for x in next(fin).split())

k = 2 ** n
table = [[0 for _ in range(k)] for _ in range(k)]
piece = 1


def fill(k, start, missing):
    global piece

    offset_i, offset_j = start
    missing_i, missing_j = missing

    if k == 2:
        if missing_i == 0:
            if missing_j == 0:
                table[offset_i + 1][offset_j] = piece
                table[offset_i][offset_j + 1] = piece
                table[offset_i + 1][offset_j + 1] = piece
            else:
                table[offset_i][offset_j] = piece
                table[offset_i + 1][offset_j] = piece
                table[offset_i + 1][offset_j + 1] = piece
        else:
            if missing_j == 0:
                table[offset_i][offset_j] = piece
                table[offset_i][offset_j + 1] = piece
                table[offset_i + 1][offset_j + 1] = piece
            else:
                table[offset_i][offset_j] = piece
                table[offset_i][offset_j + 1] = piece
                table[offset_i + 1][offset_j] = piece
        piece += 1
        return

    mid = k // 2
    # Upper half
    if missing_i < mid:
        # Upper left
        if missing_j < mid:
            fill(2, (offset_i + mid - 1, offset_j + mid - 1), (0, 0))

            fill(k // 2, (offset_i, offset_j), (missing_i, missing_j))
            fill(k // 2, (offset_i, offset_j + mid), (mid - 1, 0))
            fill(k // 2, (offset_i + mid, offset_j), (0, mid - 1))
            fill(k // 2, (offset_i + mid, offset_j + mid), (0, 0))
        # Upper right
        else:
            fill(2, (offset_i + mid - 1, offset_j + mid - 1), (0, 1))

            fill(k // 2, (offset_i, offset_j), (mid - 1, mid - 1))
            fill(k // 2, (offset_i + mid, offset_j), (0, mid - 1))
            fill(k // 2, (offset_i, offset_j + mid), (missing_i, missing_j - mid))
            fill(k // 2, (offset_i + mid, offset_j + mid), (0, 0))
    # Lower half
    else:
        # Lower left
        if missing_j < mid:
            fill(2, (offset_i + mid - 1, offset_j + mid - 1), (1, 0))

            fill(k // 2, (offset_i, offset_j), (mid - 1, mid - 1))
            fill(k // 2, (offset_i + mid, offset_j), (missing_i - mid, missing_j))
            fill(k // 2, (offset_i, offset_j + mid), (mid - 1, 0))
            fill(k // 2, (offset_i + mid, offset_j + mid), (0, 0))
        # Lower right
        else:
            fill(2, (offset_i + mid - 1, offset_j + mid - 1), (1, 1))

            fill(k // 2, (offset_i, offset_j), (mid - 1, mid - 1))
            fill(k // 2, (offset_i + mid, offset_j), (0, mid - 1))
            fill(k // 2, (offset_i, offset_j + mid), (mid - 1, 0))
            fill(k // 2, (offset_i + mid, offset_j + mid), (missing_i - mid, missing_j - mid))


fill(k, (0, 0), missing)

for row in table:
    for elem in row:
        print(elem, end=' ')
    print()
