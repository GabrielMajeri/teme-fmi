import numpy as np
from sys import exit


def compute_upper_triangular(A, b, partial_pivot=False, full_pivot=False):
    """Aduce un sistem la forma superior triunghiulară
    folosind Gauss cu/fără pivotare totală/parțială.
    """
    if partial_pivot and full_pivot:
        raise Exception("Se poate folosi doar un tip de pivotare")

    N = A.shape[0]

    # Formez matricea sistemului
    M = np.concatenate((A, b), axis=1)

    indices = np.arange(0, N)

    for k in range(N - 1):
        if partial_pivot:
            # Găsesc indicele elementului de valoare maximă
            index = np.argmax(M[k:, k])

            # Pivotez
            M[[k, index]] = M[[index, k]]
        elif full_pivot:
            submatrix = M[k:, k:N - 1]
            index = np.unravel_index(np.argmax(submatrix), submatrix.shape)

            # Obțin indicii în matricea mare
            index = (index[0] + k, index[1] + k)

            M[[k, index[0]]] = M[[index[0], k]]
            M[:, [k, index[1]]] = M[:, [index[1], k]]

            indices[[k, index[1]]] = indices[[index[1], k]]

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


def solve_upper_triangular(U, C, print_steps=False):
    """Rezolvă un sistem în formă superior triunghiulară,
    folosind substituția descendentă.
    """
    N = U.shape[0]

    x = np.zeros(N)

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

    return x


def solve_system(A, b, pivot=None):
    "Rezolvă un sistem folosind metoda Gauss."

    determinant = np.linalg.det(A)

    if np.abs(determinant) < 1e-14:
        # Sistemul nu are soluție
        return None

    partial_pivot = False
    full_pivot = False

    if pivot == "full":
        full_pivot = True
    elif pivot == "partial":
        partial_pivot = True

    M, indices = compute_upper_triangular(A, b, partial_pivot=partial_pivot, full_pivot=full_pivot)

    N = A.shape[0]
    U = M[:,:N]
    C = M[:,N]

    x = solve_upper_triangular(U, C)
    x = x[indices]

    return x


def compute_determinant(A):
    N = A.shape[0]
    b = np.zeros((N, 1))
    M, _ = compute_upper_triangular(A, b)

    det = 1
    for i in range(N):
        det *= M[i][i]
    return det

if __name__ == '__main__':
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

    A = np.array([
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 10]
    ], dtype=float)
    b = np.array([
        [4],
        [4],
        [7]
    ], dtype=float)


    det = compute_determinant(A)
    print("Determinant:", det)

    x = solve_system(A, b, pivot="full")
    print("Soluția:", x)


    print()
    for k in range(1, 20):
        C = 10.0 ** (-k)
        print("C:", C)
        A = np.array([
            [1, C],
            [1, 1],
        ])
        b = np.array([
            [C],
            [2],
        ])
        x = solve_system(A, b, pivot="full")
        print("Soluția:", x)
