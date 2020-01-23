"""
Se dă o matrice de NxM numere întregi.

Robotul primește sau pierde puncte, în funcție de valoarea numărului din căsuță.

Robotul se poate deplasa din celula (i, j) doar în celula (i + 1, j) sau (i, j - 1).

Robotul poate vizita un careu doar dacă a acumulat un număr strict pozitiv de puncte
până la acel moment.

Robotul pleacă din colțul dreapta-sus și vrea să ajungă în colțul dreapta-jos.
La final trebuie să aibă un număr de puncte mai mare sau egal cu `sfinal`.

Robotul poate începe cu un număr de puncte. Determinați numărul minim de puncte
cu care trebuie să înceapă ca să ajungă la final și un traseu posibil.

Rezolvare:
  Construiesc o matrice `smin` cu următoarea semnificație:
    smin[i][j] = suma minimă de care are nevoie robotul, dacă ar pleca de pe
                 poziția (i, j), ca să ajungă în colțul stânga-jos al matricei.
"""

# Citesc datele
with open("date.in") as fin:
    line = fin.readline()
    n, m, sfinal = map(int, line.split())

    matrix = []
    for _ in range(n):
        line = fin.readline()
        row = [int(x) for x in line.split()]
        matrix.append(row)

# Inițializez `smin`
smin = [[0] * m for _ in range(n)]

# Dacă robotul pleacă de pe ultima căsuță, trebuie să vedem
# cât mai punem în plus la valoarea din căsuță ca să ajungă la sfinal
smin[-1][0] = sfinal - matrix[-1][0]

# Ultima linie
for j in range(1, m):
    smin[-1][j] = smin[-1][j - 1] - matrix[-1][j]

# Prima coloană
for i in range(n - 2, -1, -1):
    smin[i][0] = smin[i + 1][0] - matrix[i][0]

# Restul matricei
for i in range(n - 2, -1, -1):
    for j in range(1, m):
        points = matrix[i][j]

        left = smin[i][j - 1]
        down = smin[i + 1][j]
        best = min(left, down)

        # În acest caz, robotul ar putea începe cu puncte negative
        # și tot ar ajunge la final. Dar trebuie ca tot timpul să aibă
        # un număr strict pozitiv de puncte.
        if best <= 0:
            if matrix[i][j] < 0:
                smin[i][j] = -points + 1
            else:
                smin[i][j] = 1
        else:
            smin[i][j] = best - points


# Răspunsul se află în colțul dreapta-sus al lui smin
print(smin[0][-1])

i, j = 0, m - 1
while i != n - 1 and j != 0:
    print(matrix[i][j], end=' ')

    # Aleg valoarea minimă
    if smin[i][j - 1] < smin[i + 1][j]:
        j = j - 1
    else:
        i = i + 1

# Am ajuns pe prima coloană, doar mergem în jos
while i != n - 1:
    print(matrix[i][j], end=' ')
    i = i + 1

# Am ajuns pe ultima linie, doar mergem la stânga
while j != 0:
    print(matrix[i][j], end=' ')
    j = j - 1

# Ultimul element, valoarea din stânga-jos
print(matrix[-1][0])
