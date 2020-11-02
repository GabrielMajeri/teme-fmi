import math

import numpy as np
import matplotlib.pyplot as plt


def create_plot():
    "Funcție ajutătoare pentru crearea graficului."
    # Creez un subplot
    fig, ax = plt.subplots(1, dpi=200)

    # Setez axele de coordonate
    ax.spines['bottom'].set_position('zero')
    ax.spines['top'].set_color('none')
    ax.spines['right'].set_color('none')

    return ax


###
print("Exercițiul 1")

# Pentru a aproxima valoarea lui radical din 8,
# căutăm soluția pozitivă a ecuației x^2 - 8 = 0
f = lambda x: x ** 2 - 8

# Metoda bisecției, dezvoltată la primul laborator
def bisectie(f, a, b, epsilon=1e-8):
    """Găsește rădăcina funcției `f` pe intervalul `[a, b]`
    cu precizia `epsilon`.
    """

    # Calculăm valorile în capătul stâng
    f_a = f(a)

    # Prima estimare, mijlocul intervalului inițial
    x_num = (a + b) / 2

    # Numărul necesar de iterații
    num_iterations = math.floor(math.log2((b - a) / epsilon) - 1) + 1

    # Aplicăm algoritmul
    for step in range(num_iterations):
        value = f(x_num)

        # Am găsit fix valoarea căutată, ieșim
        if value == 0:
            break
        elif f_a * value < 0:
            b = x_num
        else:
            a = x_num

        # Luăm mijlocul noului interval
        x_num = (a + b) / 2

    return x_num

# Căutăm rădăcina pozitivă
root = bisectie(f, 0, 8)

print("Radical din 8 este", root)
print()

# Afișez funcția folosită pentru calcularea radicalului
xs = np.linspace(0, 8, int(1e5))

ax = create_plot()
ax.set_title("Găsirea rădăcinii pătrate")

ax.plot(xs, f(xs), label="x^2 - 8")
ax.scatter(root, 0, c='red')

ax.legend()
plt.show()


###
print("Exercițiul 2")

# Definesc cele două părți ale egalității
f = lambda x: np.exp(x - 2)
g = lambda x: np.cos(np.exp(x - 2)) + 1

h = lambda x: f(x) - g(x)

# Găsim soluția ecuației h(x) = 0, care va fi și soluția ecuației inițiale
# Intervalul [1, 3] a fost ales analizând graficul
root = bisectie(h, 1, 3)
print("Funcțiile se intersectează în", root)

# Afișăm grafic rezultatul
xs = np.linspace(0, 5, int(1e5))

ax = create_plot()
ax.set_title("Rezolvarea unei ecuații")

ax.plot(xs, f(xs), label="e^(x - 2)")
ax.plot(xs, g(xs), label="cos(e^(x - 2)) + 1")
ax.scatter(root, f(root), c='red')

ax.legend()
plt.show()

print()


###
print("Exercițiul 3")

def pozitie_falsa(f, a, b, epsilon=1e-5):
    """Găsește soluția ecuației `f(x) = 0` folosind metoda poziției false
    în intervalul [a, b], cu precizie `epsilon`.
    """

    f_a = f(a)
    f_b = f(b)

    prev = (a * f_b - b * f_a)/(f_b - f_a)
    f_prev = f(prev)

    # Setez o valoare lui new, pentru cazul când f_prev == 0
    # și ies imediat din buclă.
    new = prev

    num_iterations = 0
    while True:
        # Dacă am nimerit soluția exactă, mă opresc
        if f_prev == 0:
            break
        elif f_a * f_prev < 0:
            # Intervalul bun e cel din stânga
            b = prev
            f_b = f(b)
        else:
            # Intervalul bun e cel din dreapta
            a = prev
            f_a = f(a)

        # Calculez noul punct de intersecție
        new = (a * f_b - b * f_a)/(f_b - f_a)
        f_new = f(new)

        if np.abs(new - prev) / np.abs(prev) < epsilon:
            break

        prev, f_prev = new, f_new
        num_iterations += 1

    return new, num_iterations


f = lambda x: (x ** 3) - 19 * x + 30

# Calculez rădăcinile
# Intervalele au fost alese pe baza graficului
r1, n1 = pozitie_falsa(f, -6, -4)
print("Am găsit soluția", r1, "în", n1, "iterații")
r2, n2 = pozitie_falsa(f, 1, 2.5)
print("Am găsit soluția", r2, "în", n2, "iterații")
r3, n3 = pozitie_falsa(f, 2.5, 4)
print("Am găsit soluția", r3, "în", n3, "iterații")

xs = np.linspace(-5, 5, int(1e5))

ax = create_plot()
ax.set_title("Metoda poziției false")

# Afișez funcția
ax.plot(xs, f(xs), label='x^3 - 19x + 30')
# Afișez rădăcinile
ax.scatter([r1, r2, r3], [0, 0, 0], c='red')

ax.legend()
plt.show()

print()


###
print("Exercițiul 4")

def secanta(f, a, b, x0, x1, epsilon=1e-5):
    """Găsește rădăcina lui f pe intervalul [a, b],
    plecând de la punctele x0 și x1.
    """
    num_iterations = 0
    # Ne oprim când eroarea relativă scade sub epsilon
    while np.abs(x1 - x0) / np.abs(x0) >= epsilon:
        # Calculăm următorul punct folosind secanta
        x_new = (x0 * f(x1) - x1 * f(x0)) / (f(x1) - f(x0))

        if x_new < a or x_new > b:
            raise Exception("Valorile alese pentru x0 și x1 nu converg")

        x0, x1 = x1, x_new
        num_iterations += 1

    return x1, num_iterations


f = lambda x: (x ** 3) - 7 * x + 6


a, b = -3, 3

r1, n1 = secanta(f, a, b, -3, -2.5)
print("Am găsit soluția", r1, "în", n1, "iterații")
r2, n2 = secanta(f, a, b, 0.5, 1.5)
print("Am găsit soluția", r2, "în", n2, "iterații")
r3, n3 = secanta(f, a, b, 1.5, 2.5)
print("Am găsit soluția", r3, "în", n3, "iterații")

xs = np.linspace(a, b, int(1e5))

ax = create_plot()
ax.set_title("Metoda secantei")

# Afișez funcția
ax.plot(xs, f(xs), label='x^3 - 7x + 6')
# Afișez rădăcinile
ax.scatter([r1, r2, r3], [0, 0, 0], c='red')

ax.legend()
plt.show()
