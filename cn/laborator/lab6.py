import numpy as np
import scipy.linalg as la


# Matricea dată la intrare
A = np.array([
    [2, -1, -2],
    [4, 2, 0],
    [0, -2, -1],
], dtype=np.float64)


A = np.array([
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 10],
], dtype=np.float64)


N = A.shape[0]
P = np.eye(N)
L = np.zeros((N, N))
U = np.copy(A)

partial_pivot = True

for k in range(N - 1):
    if partial_pivot:
        # Găsesc indicele elementului de magnitudine maximă
        index = k + np.argmax(np.abs(U[k:, k]))

        # Pivotez
        U[[k, index]] = U[[index, k]]
        L[[k, index]] = L[[index, k]]

        # Interschimb în permutare
        P[[k, index]] = P[[index, k]]

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

L += np.eye(N)

print("L = ")
print(L)
print("U = ")
print(U)
print("L @ U = ")
print(L @ U)
print("P @ A = ")
print(P @ A)
