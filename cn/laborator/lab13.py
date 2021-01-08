import numpy as np
import matplotlib.pyplot as plt


## Metodă numerică de calcul a derivatei unei funcții

def f(x):
    return np.sin(x)

left, right = -np.pi, +np.pi

N = 30

x = np.linspace(left, right, N)
y = f(x)

h = x[1] - x[0]
interior = x[1:-1]


# Calculez derivata și aproximările ei
derivative = np.cos(interior)
progressive_diffs = (y[2:] - y[1:-1]) / h
regressive_diffs = (y[1:-1] - y[0:-2]) / h
central_diffs = (y[2:] - y[0:-2]) / (2 * h)

plt.figure(dpi=150)
plt.suptitle('Aproximarea derivatei')
plt.plot(interior, derivative, label='Derivata exactă')
plt.plot(interior, progressive_diffs, label='Diferențe progresive')
plt.plot(interior, regressive_diffs, label='Diferențe regresive')
plt.plot(interior, central_diffs, label='Diferențe centrale')
plt.legend()
plt.show()

# Afișez erorile de trunchiere
def compute_error(approx):
    return np.abs(approx - derivative)

plt.figure(dpi=150)
plt.suptitle('Eroare de trunchiere')
plt.plot(interior, compute_error(progressive_diffs), label='Diferențe progresive')
plt.plot(interior, compute_error(regressive_diffs), label='Diferențe regresive')
plt.plot(interior, compute_error(central_diffs), label='Diferențe centrale')
plt.legend()
plt.show()
