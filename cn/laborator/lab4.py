import numpy as np
from sys import exit


## Rezolvarea unui sistem prin metoda Gauss

## Datele de intrare
# Matricea coeficienților
A = np.array([
    [2, -1, -2],
    [4, 2, 0],
    [0, -2, -1]
], dtype=float)

# Matricea termenilor liberi
b = np.array([
    [-1],
    [6],
    [-3]
], dtype=float)

determinant = np.linalg.det(A)

if np.abs(determinant) < 1e-14:
    print("Sistemul nu are soluție")
    exit(1)

N = A.shape[0]
# Formez matricea sistemului
M = np.concatenate((A, b), axis=1)

# Activează pivotarea parțială
partial_pivot = False

for k in range(N - 1):
    if partial_pivot:
        # Găsesc indicele elementului de valoare maximă
        index = np.argmax(M[k:, k])

        # Pivotez
        M[[k, index]] = M[[index, k]]

    # Selectez coloana
    ratios = M[k + 1:, k]

    # Determin raportul pentru fiecare rând
    ratios = ratios / M[k, k]

    row = M[k, :]

    # Înmulțesc fiecare raport cu primul rând
    difference = np.outer(ratios, row)

    # Actualizez matricea
    M[k + 1:, :] -= difference

print("Matricea sistemului:")
print(M)

x = np.zeros(N)

U = M[:,:N]
C = M[:,N]

print_steps = False

# Merg de la ultima linie către prima,
# și rezolv pe rând ecuațiile prin substituție
for i in range(N - 1, -1, -1):
    coefs = U[i, i + 1:]
    values = x[i + 1:]

    x[i] = (C[i] - coefs @ values) / U[i, i]

    if print_steps:
        print("Pasul", N - i, ":",
            C[i], "-", coefs, "@", values, ")",
            "/", U[i, i])

print("Soluția: ", x)
