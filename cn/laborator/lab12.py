import numpy as np
import matplotlib.pyplot as plt


## Interpolare spline pătratică

# Definesc funcția
def f(x):
    return np.cos(2 * x) - 2 * np.sin(3 * x)

def f_derivat(x):
    return -2 * np.sin(2 * x) - 6 * np.cos(3 * x)

left = -np.pi
right = np.pi


# Numărul de funcții folosite în interpolare
N = 12

# Iau N + 1 puncte egal depărtate
x = np.linspace(left, right, N + 1)
# Eșantionez funcția
y = f(x)

# Distanța dintre punctele consecutive este constantă
h = x[1] - x[0]

prev_b = 0

def construieste_functie_spline_patratica(i):
    global prev_b
    a = y[i]
    if i == 0:
        b = f_derivat(x[0])
    else:
        b = 2 * (y[i] - y[i - 1])/h - prev_b
    prev_b = b
    c = (y[i + 1] - y[i] - h * b)/(h ** 2)

    def spline(t):
        return a + b * (t - x[i]) + c * ((t - x[i]) ** 2)

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
        construieste_functie_spline_patratica(i)
        for i in range(N)
    ]
)

plt.plot(x_grafic, y_grafic, label='funcția')
plt.plot(x_grafic, y_aproximat, label='funcția aproximată')
plt.legend()
plt.show()
