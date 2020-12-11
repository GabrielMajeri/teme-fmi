import numpy as np
import matplotlib.pyplot as plt


## Interpolare cu polinoame Lagrange

# Definesc funcția
# def f(x):
#     return np.cos(2 * x) - 2 * np.sin(3 * x)

def f(x):
    return 1 / (1 + 25 * (x ** 2))

# Definesc domeniul pe care fac interpolarea
left = -np.pi
right = +np.pi

# Gradul polinomului de interpolare
N = 20

# Definesc polii de interpolare

use_chebyshev = True

if not use_chebyshev:
    # Polii egal distanțați
    x = np.linspace(left, right, N + 1)
else:
    # Polii Chebyshev
    def chebyshev(k):
        x = np.cos(np.pi * ((N + 1 - k) / N))
        return (left + right)/2 + ((right - left)/2) * x

    x = np.array([chebyshev(k) for k in range(1, N + 2)])

y = f(x)

def compute_ordinary_polynomial():
    # Construiesc matricea Vandermonde pentru sistemul pe care vreau să-l rezolv
    A = np.vander(x, N=N + 1, increasing=True)

    a = np.linalg.solve(A, y)

    def polynomial(x):
        "Evaluează polinomul cu coeficienții `a` în punctul `x`"
        s = 0
        for i in range(N + 1):
            s += a[i] * (x ** i)
        return s

    return polynomial


def compute_lagrange_polynomial():
    def lagrange_coef(k, t):
        coefs = []
        for i in range(N + 1):
            if i != k:
                coefs.append((t - x[i])/(x[k] - x[i]))
        return np.prod(coefs)

    def polynomial(x):
        "Evaluează polinomul obținut cu interpolare Lagrange în punctul `x`"
        s = 0
        for i in range(N + 1):
            s += lagrange_coef(i, x) * y[i]
        return s

    return polynomial

def polynomial_vector(polynomial, v):
    "Evaluează polinomul dat pe vectorul `v`"
    return np.array([polynomial(x) for x in v])


# Afișez funcția
xs = np.linspace(left, right, 200)
plt.plot(xs, f(xs), linestyle='-', label='funcția f')

# Afișez polii interpolării
plt.scatter(x, y, c='red')

# Afișez interpolările

ordinary = compute_ordinary_polynomial()
plt.plot(xs, polynomial_vector(ordinary, xs),
    c='orange',
    linestyle='--',
    label=f'aproximare cu polinom de gradul {N}')

lagrange = compute_lagrange_polynomial()
plt.plot(xs, polynomial_vector(lagrange, xs),
    c='purple',
    linestyle='-.',
    label=f'aproximare cu Lagrange de gradul {N}')

# Afișez o legendă
plt.legend()

# Afișez tot graficul
plt.show()


# Afișăm eroarea aproximării
show_error = True
if show_error:
    y_real = f(xs)

    y_approx = polynomial_vector(ordinary, xs)
    plt.plot(xs, np.abs(y_approx - y_real),
        linestyle='--',
        label=f'eroare pentru N={N}')

    y_approx = polynomial_vector(lagrange, xs)
    plt.plot(xs, np.abs(y_approx - y_real),
        linestyle='-.',
        label=f'eroare pentru Lagrange N={N}')

    plt.legend()
    plt.show()
