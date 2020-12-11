import numpy as np


## Metoda coborârii pe gradient

def l2_norm(x):
    "Calculează norma L2 pentru vectorul `x`"
    return np.linalg.norm(x, ord=2)

def positive_semidefinite(M):
    "Verifică ca matricea `M` să fie pozitiv semi-definită."
    for i in range(M.shape[0]):
        # Folosesc criteriul lui Sylvester,
        # calculez determinanții minorilor principali
        if np.linalg.det(M[:i, :i]) < 0:
            return False
    return True

## Datele de intrare
# Matricea coeficienților
A = np.array([
    [3, 2],
    [2, 6]
], dtype=float)

# Matricea termenilor liberi
b = np.array([
    [2],
    [-8]
], dtype=float)


if not positive_semidefinite(A):
    print("Matricea A nu este pozitiv semidefinită!")
else:
    # Inițializez cu valori random
    x = np.random.randn(*b.shape)

    residue = b - A @ x

    # Ne oprim dacă norma reziduului scade sub această valoare
    STOPPING_EPSILON = 1e-10

    num_iterations = 0
    while l2_norm(residue) > STOPPING_EPSILON:
        learning_rate = (residue.T @ residue) / (residue.T @ A @ residue)

        x = x + learning_rate * residue
        residue = b - A @ x

        num_iterations += 1

    print("x =", x[:, 0])
