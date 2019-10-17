"""
a) Demonstrație de corectitudine:
1. Algoritmul greedy produce soluții
2. Soluția greedy este optimă

1. La fiecare pas inserez într-un subșir astfel încât acela rămâne descrescător,
sau creez un nou subșir dacă nu găsesc

2. Presupun că soluția greedy diferă de soluția optimă.
Să zicem că soluția greedy reține subșirurile pe care le construiește
de la stânga la dreapta, ordonate după ultimul element din subșir.

Fie x_i primul element care diferă.

Soluția greedy l-a inserat pe „cel mai din stânga” subșir posibil
(adică în primul subșir care avea capătul mai mare sau egal cu x_i).

Soluția optimă trebuie să îl insereze într-un subșir „mai la dreapta”
(adică într-un subșir în care avea voie să fie pus).

Dar noi putem interschimba ce numere au venit în soluția optimă după x_i
cu ce numere au venit în soluția greedy. Și astfel am obține o soluție
mai bună decât cea optimă.
"""

from bisect import bisect
import heapq

lines = (line.strip() for line in open('descomp_desc.txt'))

n = int(next(lines))
nums = map(int, next(lines).split())

subintervals = []
tails = []

# Parcurg fiecare număr o singură dată - O(n)
for value in nums:
    # print('Now processing', value)

    # Găsesc punctul de inserție prin căutare binară - O(log n),
    # deoarece pot fi maxim `n` subșiruri.
    idx = bisect(tails, value)

    # print('Inserting at', idx)

    if idx == len(subintervals):
        subintervals.append([value])
        tails.append(value)
    elif tails[idx] >= value:
        subintervals[idx].append(value)
        tails[idx] = value

print(*subintervals, sep='\n')

heap = [(stack.pop(), idx) for idx, stack in enumerate(subintervals)]
heapq.heapify(heap)

sorted_vec = []

while heap:
    value, idx = heapq.heappop(heap)
    sorted_vec.append(value)

    if subintervals[idx]:
        heapq.heappush(heap, (subintervals[idx].pop(), idx))

print(*sorted_vec)
