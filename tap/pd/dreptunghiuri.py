"""
Se dau N dreptunghiuri, reprezentate de lățimile și înălțimile lor.
Trebuie puse pe linii de lungime cel mult L.

Înălțimea unei linii este maximul înălțimilor dreptunghiurilor de pe acea linie.
După ce o linie se umple, se începe o nouă linie sub aceasta.

Obiectivul este să se așeze toate dreptunghiurile, în ordinea în care
au fost date, pe linii, astfel încât suma înălțimilor liniilor să fie minimă.
"""

from collections import namedtuple


Rectangle = namedtuple("Rectangle", ("width", "height"))

# Citesc datele
rectangles = []
with open("dreptunghiuri.txt") as fin:
    n = int(fin.readline())

    max_length = int(fin.readline())

    for _ in range(n):
        line = fin.readline()
        width, height = map(int, line.split())
        rectangles.append(Rectangle(width, height))

# Vector care reține înălțimea totală minimă care se poate obține cu
# primele i dreptunghiuri.
min_height = [0] * n

# Primul dreptunghi începe prima linie
min_height[0] = rectangles[0].height

for i in range(1, n):
    # Încerc să găsesc linia optimă pentru dreptunghiul cu indicele i
    current = rectangles[i]

    # Presupun că l-aș pune pe o nouă linie și încerc să caut o înălțime mai mică
    min_height[i] = min_height[i - 1] + current.height

    for j in range(1, i + 1):
        # Dacă mai are loc pe linie
        if sum(rect.width for rect in rectangles[j:i+1]) <= max_length:
            # Determin cât ar veni înălțimea dacă îl pun pe linie
            new_line_height = max(rect.height for rect in rectangles[j:i + 1])

            # Verific dacă soluția e mai bună
            new_min_height = min_height[j - 1] + new_line_height

            min_height[i] = min(min_height[i], new_min_height)


# Soluția este înălțimea minimă pe care o pot obține dacă
# iau în considerare toate dreptunghiurile
print(min_height[-1])
