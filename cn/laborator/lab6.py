import numpy as np
import scipy.linalg as la


# Matricea dată la intrare
A = np.array([
    [2, -1, -2],
    [4, 2, 0],
    [0, -2, -1],
], dtype=np.float64)

N = A.shape[0]
L = np.eye(N)
U = np.copy(A)

for k in range(N - 1):
    # Selectez coloana pe care lucrez
    ratios = U[k + 1:, k]

    # Determin raportul pentru fiecare rând
    ratios = ratios / U[k, k]

    # Actualizez matricea inferior triunghiulară
    L[k + 1:, k] = ratios

    # Selectez rândul pe care vreau să-l actualizez
    row = U[k, :]

    # Înmulțesc fiecare raport cu primul rând
    difference = np.outer(ratios, row)

    # Actualizez matricea superior triunghiulară
    U[k + 1:, :] -= difference

print("L = ")
print(L)
print("U = ")
print(U)
print("L @ U = ")
print(L @ U)
