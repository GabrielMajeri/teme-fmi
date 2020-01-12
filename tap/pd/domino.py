"""
Se dă un șir de dominouri de câte două caractere.

    ab, pq, bc, qb, bx, xt

Vrem să construim un lanț maxim de dominouri.

    Lanț de lungime 3:
        pq qb bx

Idee de rezolvare în O(n):
Formăm un dicționar L indexat după litere,
unde L[ch] = lungimea maximă a unui lanț care se începe cu `ch`
"""

with open('domino.txt') as fin:
    n = int(next(fin))

    dominos = []
    for _ in range(n):
        a, b = map(int, next(fin).split())

        dominos.append((a, b))

print(*dominos)

max_lens = {}
preds = {}

for i in reversed(range(n)):
    start, end = dominos[i]
    prev_len = max_lens.get(start, 0)
    max_len = max_lens.get(end, 0)

    max_lens[start] = max(prev_len, 1 + max_len)
    preds[start] = i

print(max_lens)
