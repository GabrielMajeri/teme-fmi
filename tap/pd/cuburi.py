"""
Se dă o listă de cuburi de latură l_i și culoare c_i.

Să se construiască un turn de înălțime maximă astfel încât
laturile cuburilor succesive sunt în ordine crescătoare și
culorile cuburilor alăturate sunt diferite.

Laturile _nu_ sunt distincte.

Să se găsească înălțimea maximă posibilă a unui turn și
numărul de moduri în care se poate obține acel turn.

Sortăm cuburile descrescător după latură.

Definim:
 - H[i] = înălțimea maximă a unui turn care are la bază cubul i
 - nr[i] = câte turnuri de înălțime H[i] se pot forma având la bază cubul

Inițializăm cuburile de latură minimă:
 - înălțimea = înălțimea cubului
 - numărul de turnuri = 1

Exemplu:
(9, 3) (8, 1) (8, 2) (7, 3) (7, 1) (6, 1) (5, 2) (3, 4) (3, 5)
  38     29       29     21     15     14      8     3      3
   4      2        2      2      2      2      2     1      1

Recurența este:
 - nr[i] = sumă de nr[j] pentru j unde
                                    H[i] = H[j] + latura cubului i,
                                  și culoarea cubului i != culoarea cubului j
 - H[i] = max(H[j] + latura lui i) pentru j unde
                                   pot adăuga cubul i peste cubul j
"""

from typing import NamedTuple


class Cube(NamedTuple):
    length: int
    color: int

    def __repr__(self):
        return f'{self.length} {self.color}'


cubes = []
with open('cuburi.txt') as fin:
    n, _ = map(int, next(fin).split())
    for _ in range(n):
        line = next(fin)
        length, color = map(int, line.split())
        cubes.append(Cube(length, color))

cubes.sort()

max_heights = [cubes[i].length for i in range(n)]
max_counts = [1 for _ in range(n)]
preds = [-1 for _ in range(n)]

for i in range(n):
    max_height = cubes[i].length

    for j in range(i):
        height = cubes[i].length + max_heights[j]
        if cubes[i].color != cubes[j].color and cubes[i].length != cubes[j].length:
            if height > max_height:
                max_height = height
                preds[i] = j

    max_heights[i] = max_height

    if max_height == cubes[i].length:
        max_counts[i] = 1
    else:
        max_count = 0

        for j in range(i):
            if cubes[i].color != cubes[j].color and max_height == max_heights[j] + cubes[i].length:
                max_count += max_counts[j]

        max_counts[i] = max_count

max_height = 0
max_idx = -1

for idx, height in enumerate(max_heights):
    if height > max_height:
        max_height = height
        max_idx = idx

current_idx = max_idx

print('Turn:')
while current_idx != -1:
    print(cubes[current_idx])
    current_idx = preds[current_idx]


print('Număr de turnuri:')
print(sum(max_counts[i] for i in range(n) if max_heights[i] == max_height))
