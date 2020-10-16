import numpy as np
import matplotlib.pyplot as plt


def newton_rhapson(f, df, x0, epsilon=1e-5):
    """Găsește o soluție a funcției f cu derivata df, aplicând
    metoda lui Newton, pornind din punctul x0.
    """
    # Primul punct este cel primit ca parametru
    prev_x = x0
    # Aplicăm prima iterație
    x = x0 - f(x0) / df(x0)

    # Continuăm să calculăm până avem precizia cerută.
    num_steps = 1
    while abs(x - prev_x) / abs(prev_x) > epsilon:
        x, prev_x = x - f(x) / df(x), x

        num_steps += 1

    return x, num_steps


# Funcția pentru care vrem să găsim soluții.
f = lambda x: (x ** 3) - 7 * (x ** 2) + 14 * x - 6
# Prima derivată a lui f.
df = lambda x: 3 * (x ** 2) - 14 * x + 14

s1, n1 = newton_rhapson(f, df, 0.5)
s2, n2 = newton_rhapson(f, df, 2.5)
s3, n3 = newton_rhapson(f, df, 3.5)
print("Soluția", s1, "după", n1, "pași")
print("Soluția", s2, "după", n2, "pași")
print("Soluția", s3, "după", n3, "pași")


## Afișez rezultatul
# Creez un nou plot cu rezoluție mare
fig, ax = plt.subplots(1, dpi=200)

# Pun un titlu
plt.title('Metoda Newton-Rhapson')

# Configurez axele
ax.spines['bottom'].set_position('zero')
ax.spines['top'].set_color('none')
ax.spines['right'].set_color('none')

# Generez punctele
x = np.linspace(start=0, stop=5, num=1000)

# Desenez graficul funcției
plt.plot(x, f(x), '-.')

# Desenez soluțiile
plt.scatter([s1, s2, s3], [0, 0, 0], c='red')

# Afișez legenda
plt.legend(['f(x)', 'x_num'])

# Etichetez axele
plt.xlabel('x')
plt.ylabel('y')

plt.show()
