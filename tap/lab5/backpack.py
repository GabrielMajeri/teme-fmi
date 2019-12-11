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

    for index in starting_items:
        current_cost += items[index].cost
        current_weight += items[index].weight

    if current_weight > max_weight:
        return -1, -1

    sol_disc = current_cost
    sol_frac = current_cost

    i = 0
    while i < n:
        if i in starting_items:
            i += 1
            continue

        if i in forbidden_items:
            i += 1
            continue

        ratio, cost, weight = items[i]

        if current_weight + weight <= max_weight:
            current_cost += cost
            current_weight += weight

            sol_disc = current_cost
            sol_frac = current_cost
        else:
            remaining_weight = max_weight - current_weight
            frac = remaining_weight / weight
            sol_frac += cost * frac
            break

        i += 1

    return sol_disc, sol_frac


class Solution:
    def __init__(self, level, starting, forbidden):
        self.level = level
        self.starting = starting
        self.forbidden = forbidden
        self.sol_disc, self.sol_frac = solve_greedy(starting, forbidden)

    def __lt__(self, other):
        return self.sol_frac > other.sol_frac


solutions = [
    Solution(0, [], [])
]
best_sol_disc = solutions[0].sol_disc
counter = 1

while solutions:
    solution = heappop(solutions)

    print(counter, f'cf={solution.sol_frac}', f'cd={solution.sol_disc}', solution.starting, solution.forbidden)
    counter += 1

    if solution.sol_disc > best_sol_disc:
        best_sol_disc = solution.sol_disc

    if solution.sol_frac < best_sol_disc:
        continue

    if solution.sol_frac == best_sol_disc:
        break

    item_idx = solution.level

    if item_idx == n:
        continue

    heappush(solutions, Solution(solution.level + 1, solution.starting + [item_idx], solution.forbidden))
    heappush(solutions, Solution(solution.level + 1, solution.starting, solution.forbidden + [item_idx]))

print('Best:', best_sol_disc)
