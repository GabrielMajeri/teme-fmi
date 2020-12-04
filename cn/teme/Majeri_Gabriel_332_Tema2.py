import numpy as np


# Toleranța pentru comparații cu 0.
EPSILON = 1e-11


def gauss_pivotare_totala(M):
    """Transformă matricea M într-una superior triunghiulară,
    aplicând metoda Gauss cu pivotare totală.

    Returnează matricea finală și permutarea aplicată pe coloane.
    """
    N = M.shape[0]
    M = M.copy()

    # Rețin permutarea care se va aplica necunoscutelor
    indices = np.arange(0, N)

    for k in range(N - 1):
        # Submatricea în care lucrez
        submatrix = M[k:, k:N - 1]

        # Caut elementul de valoare maximă
        index = np.argmax(np.abs(submatrix))
        i, j = np.unravel_index(index, submatrix.shape)

        # Obțin indicii în matricea mare
        i += k
        j += k

        # Interschimb liniile
        M[[k, i], :] = M[[i, k], :]
        # Interschimb coloanele
        M[:, [k, j]] = M[:, [j, k]]
        # Rețin permutarea necunoscutelor
        indices[k], indices[j] = indices[j], indices[k]

        # Selectez coloana
        ratios = M[k + 1:, k]

        # Determin raportul pentru fiecare rând
        ratios = ratios / M[k, k]

        row = M[k, :]

        # Înmulțesc fiecare raport cu primul rând
        difference = np.outer(ratios, row)

        # Actualizez matricea
        M[k + 1:, :] -= difference

    return M, indices

def descompunere_lu(A):
    """Descompune matricea `A` într-un produs dintre o matrice
    inferior triunghiulară și una superior triunghiulară.

    Returnează matricea inf. tri., cea sup. tri. și matricea care reprezintă
    permutarea aplicată matricii `A`.
    """
    N = A.shape[0]

    # Inițial am permutarea identică
    P = np.eye(N)

    # Matriciile L și U
    L = np.zeros((N, N))
    U = np.copy(A)

    for k in range(N - 1):
        # Găsesc indicele elementului de magnitudine maximă
        index = k + np.argmax(np.abs(U[k:, k]))

        # Pivotez
        U[[k, index]] = U[[index, k]]
        L[[k, index]] = L[[index, k]]

        # Interschimb liniile și în permutare
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

    return L, U, P


def substitutie_descendenta(U, b):
    N = b.shape[0]
    x = np.zeros(N)

    for i in range(N - 1, -1, -1):
        coefs = U[i, i + 1:]
        values = x[i + 1:]

        x[i] = (b[i] - coefs @ values) / U[i, i]

    return x

def substitutie_ascendenta(L, b):
    N = b.shape[0]
    x = np.zeros(N)

    for i in range(0, N):
        coefs = L[i, :i + 1]
        values = x[:i + 1]

        x[i] = (b[i] - coefs @ values) / L[i, i]

    return x

def simetrica(M):
    "Verifică dacă matricea `M` este simetrică."
    return np.all(M == M.T)

def pozitiv_semidefinita(M):
    "Verifică dacă matricea M este pozitiv-semidefinită."
    for i in range(M.shape[0]):
        minor = M[:i, :i]
        # Dacă cel puțin un minor principal nu are determinantul nenegativ,
        # nu este pozitiv-semidefinită.
        if np.linalg.det(minor) < 0:
            return False
    return True

def descompunere_cholesky(M):
    N = M.shape[0]

    L = np.eye(N)

    for i in range(N):
        L_i = np.eye(N)

        pivot = M[i, i]
        L_i[i:, i] = M[i:, i] / np.sqrt(pivot)

        M_nou = np.eye(N)
        outer = np.outer(M[i + 1:, i], M[i, i + 1:])
        M_nou[i + 1:, i + 1:] = M[i + 1:, i + 1:] - outer / pivot

        L = L @ L_i
        M = M_nou

    # La final, M va fi matricea identitate

    return L

###
print("Exercițiul 1")

A = np.array([
    [0, 6, 3, -3],
    [-1, 7, -10, 1],
    [7, 3, 7, 1],
    [-9, 7, -8, 0],
], dtype=float)

b = np.array([
    [33],
    [-25],
    [110],
    [-59],
], dtype=float)

if abs(np.linalg.det(A)) <= EPSILON:
    print("Sistemul nu este unic determinat")
else:
    print("Sistemul admite soluție unică")

    # Construiesc matricea completă a sistemului
    M = np.hstack((A, b))

    U, indices = gauss_pivotare_totala(M)

    solution = substitutie_descendenta(U[:, 0:-1], U[:, -1])

    # Pivotarea totală a permutat indicii;
    # trebuie să aplicăm invers permutarea
    solution[indices] = solution.copy()

    print("x =", solution)
    print("Verificare:", A @ solution, "==", b[:, 0])


print()

###
print("Exercițiul 2")

B = np.array([
    [0, -4, -8, -1],
    [7, -8, -7, -3],
    [7, -3, 2, -6],
    [0, 8, -3, -3]
], dtype=float)

if abs(np.linalg.det(B)) < EPSILON:
    print("Matricea nu este inversabilă")
else:
    print("Matricea este inversabilă")

    N = B.shape[0]
    M = np.hstack((B, np.eye(N)))
    U, indices = gauss_pivotare_totala(M)

    for i in range(N):
        # Facem să fie 1 pe diagonala matricei din stânga
        U[i] /= U[i][i]

    for i in range(1, N):
        # Gauss nu elimină și elementele de deasupra diagonalei principale,
        # trebuie să le reducem manual
        for j in range(i):
            ratio = U[j, i] / U[i, i]
            U[j] -= ratio * U[i, :]

    inversa = U[:4, 4:]

    # Aplicăm permutarea inversă
    inversa[indices] = inversa.copy()

    # Afișăm rezultatul
    print("Inversa lui B este")
    print(inversa)

    # Determinantul este aproximativ egal cu 1
    print("Verificare: det(B @ inversa) =", np.linalg.det(B @ inversa))


print()

###
print("Exercițiul 3")

A = np.array([
    [0, -9, -7, 6],
    [-6, 2, -10, 2],
    [6, -1, 0, 5],
    [-5, -1, -2, 7]
], dtype=float)

b = np.array([
    [-25],
    [-36],
    [34],
    [14]
], dtype=float)

if abs(np.linalg.det(A)) <= EPSILON:
    print("Sistemul nu este unic determinat")
else:
    print("Sistemul admite soluție unică")

    L, U, P = descompunere_lu(A)

    y = substitutie_ascendenta(L, b)
    x = substitutie_descendenta(U, y)

    print("Soluția permutată este", x)
    print("Verificare:", P @ A @ x, "==", b[:, 0])


print()

###
print("Exercițiul 4")

C = np.array([
    [81, 63, -54, 18],
    [63, 85, -54, 8],
    [-54, -54, 56, 6],
    [18, 8, 6, 46]
], dtype=float)

if simetrica(C) and pozitiv_semidefinita(C):
    print("Admite factorizare Cholesky")

    L = descompunere_cholesky(C)

    print("Matricea triunghiulară:")
    print(L)

    print("Verificare:")
    print(L @ L.T)
    print("==")
    print(C)

else:
    print("Nu admite factorizare Cholesky")
