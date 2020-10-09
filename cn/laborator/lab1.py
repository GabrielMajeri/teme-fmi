import numpy as np
import matplotlib.pyplot as plt
import math

def bisection_search(f, a, b, epsilon=1e-5):
    """Găsește rădăcina funcției `f` pe intervalul `[a, b]`
    cu precizia `epsilon`.
    """

    # Calculăm valorile în capete
    f_a = f(a)
    f_b = f(b)

    # Trebuie să aibă semne diferite pentru această metodă
    assert f_a * f_b < 0

    # Prima estimare, mijlocul intervalului inițial
    x_num = (a + b) / 2

    # Numărul necesar de iterații
    num_iterations = math.floor(1 + math.log((b - a) / epsilon))

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

        x_num = (a + b) / 2

    return x_num


# Funcția pentru care căutăm soluțiile
f = lambda x: (x ** 3) - 7 * (x ** 2) + 14 * x - 6

# Căutăm pe intervalele date
a = bisection_search(f, 0, 1)
b = bisection_search(f, 1, 3.2)
c = bisection_search(f, 3.2, 4)
print(a)
print(b)
print(c)


# Afișez rezultatul
fig, ax = plt.subplots(1, dpi=200)
plt.title('Metoda Bisecției')

# Configurez axele
ax.spines['bottom'].set_position('zero')
ax.spines['top'].set_color('none')
ax.spines['right'].set_color('none')


x = np.linspace(0, 5, 1000)
plt.plot(x, f(x), '-.')
plt.scatter([a, b, c], [0, 0, 0], c='red')

plt.legend(['f(x)', 'x_num'])

plt.xlabel('x')
plt.ylabel('y')

plt.show()
