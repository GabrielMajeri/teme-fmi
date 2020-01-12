r"""
G = 30

 c   15 50 80 45 10
 g  2.5 10 20 15  5
c/u  6   5  4  3  2

Știm că rucsacul fracționar ne oferă o margine superioară
pentru fiecare configurație.

   ob1  ob2   ob3
1  2.5  10   17.5
    6    5    4
   15 + 50   70   = 135

Arborele de posibilități:
La nivelul i, am două alegeri (0 sau 1):
fie iau, fie nu iau acel obiect

       1
    0 / \_ 1
    2      3
0 /  \ 1
4     5

Pentru fiecare nod, calculez cf și cd.
cf este estimarea (cât de bună ar putea fi soluția din acel nod),
cd este valoarea în acel nod (cât este în acel moment).

Când aleg un nod, actualizez minimul global.

Extrag tot timpul din listă nodul cu estimarea curentă minimă.

Dacă cf = cd, ne putem opri, pentru că nu există o soluție mai bună.
Nu mai avem ce să optimizăm, deoarece cd <= cf.

Reținem nodurile într-un max-heap.
"""

from collections import namedtuple
from heapq import heappush, heappop

Item = namedtuple('Item', ('ratio', 'cost', 'weight'))


with open("backpack.txt") as fin:
    max_weight = float(next(fin))

    costs = map(float, next(fin).split())
    weights = map(float, next(fin).split())

items = [Item(c/w, c, w) for c, w in zip(costs, weights)]
n = len(items)

items.sort(reverse=True)


def solve_greedy(starting_items, forbidden_items):
    current_cost = 0
    current_weight = 0

    # Trec prin toate obiectele care trebuie puse obligatoriu în rucsac și le adaug
    for index in starting_items:
        current_cost += items[index].cost
        current_weight += items[index].weight

    # Dacă deja depășesc capacitatea maximă a rucsacului cu obiectele inițiale
    if current_weight > max_weight:
        return -1, -1

    sol_disc = current_cost
    sol_frac = current_cost

    # Parcurg toate obiectele în ordine și încerc să le adaug
    i = 0
    while i < n:
        # Sari peste obiectele care sunt deja puse în rucsac
        # și peste cele pe care nu le punem
        if (i in starting_items) or (i in forbidden_items):
            i += 1
            continue

        ratio, cost, weight = items[i]

        # Dacă mai pot adăuga obiectul în rucsac
        if current_weight + weight <= max_weight:
            current_cost += cost
            current_weight += weight

            sol_disc = current_cost
            sol_frac = current_cost
        else:
            # Nu mai pot, pentru varianta fracționară mai adaug o parte din
            # ultimul obiect. Varianta discretă nu se schimbă
            remaining_weight = max_weight - current_weight
            frac = remaining_weight / weight
            sol_frac += cost * frac
            break

        # Trec la următorul obiect
        i += 1

    return sol_disc, sol_frac


class Solution:
    """Reprezintă un nod din arborele de soluții.

    Fiecare nod reține nivelul la care se află în arbore,
    ce obiecte avem deja în rucsac, și ce obiecte am decis
    să nu punem în rucsac.
    """

    def __init__(self, level, starting, forbidden):
        self.level = level
        self.starting = starting
        self.forbidden = forbidden
        # Calculez valoarea acestei soluții pentru varianta discretă și pentru cea fracționară
        self.sol_disc, self.sol_frac = solve_greedy(starting, forbidden)

    def __lt__(self, other):
        # Aleg întotdeauna soluția care pare să fie cea mai bună
        return self.sol_frac > other.sol_frac


# Inițial în coadă am doar rădăcina, adică soluția pentru vectorul inițial
solutions = [
    Solution(0, [], [])
]
# Deocamdată rădăcina este și cea mai bună soluție pe care o avem
best_sol_disc = solutions[0].sol_disc
counter = 1

# Cât timp mai am soluții de încercat în coadă
while solutions:
    solution = heappop(solutions)

    # Afișez informații despre acest nod
    print(counter, f'cf={solution.sol_frac}', f'cd={solution.sol_disc}', solution.starting, solution.forbidden)
    counter += 1

    # Dacă am găsit o soluție decât cea mai bună soluție anterioară,
    # actualizez optimul
    if solution.sol_disc > best_sol_disc:
        best_sol_disc = solution.sol_disc

    # Dacă soluția asta este mai proastă decât cea mai bună soluție găsită deja
    # atunci o sar
    if solution.sol_frac < best_sol_disc:
        continue

    # Dacă am găsit o soluție care este deja optimă / nu mai poate fi optimizată,
    # nu mai încerc să-i adaug fii în coadă
    if solution.sol_frac == best_sol_disc:
        break

    # Nivelul din arbore reprezintă la ce obiect am ajuns în vector
    item_idx = solution.level

    # Știu că arborele de soluții are maxim atâtea nivele cât are obiecte vectorul inițial
    if item_idx == n:
        continue

    # Adaug în coadă soluția în care aleg obiectul și soluția în care nu aleg obiectul
    heappush(solutions, Solution(solution.level + 1, solution.starting + [item_idx], solution.forbidden))
    heappush(solutions, Solution(solution.level + 1, solution.starting, solution.forbidden + [item_idx]))

print('Best:', best_sol_disc)
