import numpy as np
from sys import exit


# Datele de intrare
U = np.array([[2, -1, -2], [0, 4, 4], [0, 0, 1]], dtype=float)

# Vectorul coloană în care pun termenii liberi
C = np.array([[-1, 8, 1]], dtype=float).T

# Dimensiunea matricei
N = U.shape[0]

# Verific dacă are soluții
determinant = np.linalg.det(U)

if np.abs(determinant) < 1e-14:
    print("System has no solutions")
    exit(1)

x = np.zeros(N)

# Merg de la ultima linie către prima,
# și rezolv pe rând ecuațiile prin substituție
for i in range(N - 1, -1, -1):
    coefs = U[i, i + 1:]
    values = x[i + 1:]

    x[i] = (C[i] - coefs @ values) / U[i, i]

    print("Pasul", N - i, ":",
          C[i], "-", coefs, "@", values, ")",
          "/", U[i, i])

print(x)
