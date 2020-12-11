# Librariile necesare
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d

from lab5 import solve_system


## Determinarea punctului de minim a unei funcții convexe


def grid_discret(A, b):
    """
    Construieste un grid discret si evaleaza f in fiecare punct al gridului
    """

    size = 50 # Numar de puncte pe fiecare axa
    x1 = np.linspace(-4, 6, size) # Axa x1
    x2 = np.linspace(-6, 4, size) # Axa x2
    X1, X2 = np.meshgrid(x1, x2) # Creeaza un grid pe planul determinat de axele x1 si x2

    X3 = np.zeros((size, size))
    for i in range(size):
        for j in range(size):
            x = np.array([X1[i,j], X2[i,j]]) # x e vectorul ce contine coordonatele unui punct din gridul definit mai sus
            X3[i,j] = .5 * x @ A @ x - x @ b # Evaluam functia in punctul x

    return X1, X2, X3

def grafic_f(A,b):
    """
    Construieste graficul functiei f
    """

    # Construieste gridul asociat functiei
    (X1, X2, X3) = grid_discret(A, b)

    # Defineste o figura 3D
    fig1 = plt.figure()
    ax = plt.axes(projection="3d")

    # Construieste graficul functiei f folosind gridul discret X1, X2, X3=f(X1,X2)
    ax.plot_surface(X1, X2, X3, rstride=1, cstride=1, cmap='winter', edgecolor='none')

    # Etichete pe axe
    ax.set_xlabel('x1')
    ax.set_ylabel('x2')
    ax.set_zlabel('f(x1,x2)')

    # Titlu
    ax.set_title('Graficul functiei f');

    # Afiseaza figura
    plt.show()

def linii_nivel(A,b):
    """
    Construieste liniile de nivel ale functiei f
    """

    # Construieste gridul asociat functiei
    (X1, X2, X3) = grid_discret(A, b)

    # Ploteaza liniile de nivel ale functiei f
    fig2 = plt.figure()
    plt.contour(X1, X2, X3, levels = 10) # levels = numarul de linii de nivel

    # Etichete pe axe
    plt.xlabel('x1')
    plt.ylabel('x2')

    # Titlu
    plt.title('Liniile de nivel ale functiei f');

    # Afiseaza figura
    #plt.show()

# Definire functie f prin matricea A si vectorul b
A = np.array([[3, 2],[2, 6]]).astype(float) # Matrice pozitiv definita
b = np.array([[2],[-8]]).astype(float)

# Apelare functii grafic
grafic_f(A,b)
linii_nivel(A,b)

# Punctul de minim determinat prin rezolvarea sistemului A*x=b
x_num = solve_system(A, b)
print("Punct de minim:", x_num)
plt.scatter(x_num[0], x_num[1], s=50, c='black', marker='*')
plt.show()
