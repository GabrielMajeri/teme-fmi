import numpy as np
import matplotlib.pyplot as plt

## Interpolare spline liniară

# Definesc funcția
def f(x):
    return x ** 2

# def f(x):
#     return np.cos(2 * x) - 2 * np.sin(3 * x)

# Definesc domeniul pe care fac interpolarea
left = -np.pi
right = +np.pi

# Numărul de funcții folosite în interpolare
N = 4

# Iau N + 1 puncte egal depărtate
x = np.linspace(left, right, N + 1)
# Eșantionez funcția
y = f(x)

def construieste_functie_spline_liniara(i):
    a = y[i]
    b = (y[i + 1] - y[i])/(x[i + 1] - x[i])

    def spline(t):
        return a + b * (t - x[i])

    return spline

x_grafic = np.linspace(left, right, 100)

y_grafic = f(x_grafic)

y_aproximat = np.piecewise(
    x_grafic,
    [
        (x[i] <= x_grafic) & (x_grafic < x[i + 1])
        for i in range(N - 1)
    ],
    [
        construieste_functie_spline_liniara(i)
        for i in range(N)
    ]
)

plt.plot(x_grafic, y_grafic, label='funcția')
plt.plot(x_grafic, y_aproximat, label='funcția aproximată')
plt.legend()
plt.show()
