import numpy as np
import matplotlib.pyplot as plt


## Interpolare polinomială

# Definesc funcția
def f(x):
    return np.cos(2 * x) - 2 * np.sin(3 * x)


# Definesc domeniul pe care fac interpolarea
left = -np.pi
right = +np.pi

# Gradul polinomului de interpolare
N = 10

# Definesc polii de interpolare
x = np.linspace(left, right, N + 1)
y = f(x)

# Construiesc matricea Vandermonde pentru sistemul pe care vreau să-l rezolv
A = np.vander(x, N=N + 1, increasing=True)

a = np.linalg.solve(A, y)

def polynomial(x):
    "Evaluează polinomul cu coeficienții `a` în punctul `x`"
    s = 0
    for i in range(N + 1):
        s += a[i] * (x ** i)
    return s

def polynomial_vector(v):
    "Evaluează polinomul cu coeficienții `a` pe vectorul `v`"
    return np.array([polynomial(x) for x in v])


# Afișez funcția
xs = np.linspace(left, right, 5000)
plt.plot(xs, f(xs), linestyle='-', label='funcția f')

# Afișez polii interpolării
plt.scatter(x, y, c='red')

# Afișez interpolarea
xs = np.linspace(left, right, 100)
plt.plot(xs, polynomial_vector(xs),
    c='orange',
    linestyle='--',
    label=f'aproximare cu polinom de gradul {N}')

# Afișez o legendă
plt.legend()

# Afișez tot graficul
plt.show()


# Afișăm eroarea aproximării
show_error = False
if show_error:
    y_real = f(xs)
    y_approx = polynomial_vector(xs)

    plt.plot(xs, np.abs(y_approx - y_real), label=f'eroare pentru N={N}')
    plt.legend()
    plt.show()
