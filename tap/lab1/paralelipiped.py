from collections import defaultdict

fin = open('paralelipiped.txt')

lines = map(str.strip, fin)

n = int(next(lines))

sides = defaultdict(list)

for _ in range(n):
    a, b, c = sorted(map(int, next(lines).split()))
    sides[(a, b)].append(c)
    sides[(a, c)].append(b)
    sides[(b, c)].append(a)

max_height = 0
for heights in sides.values():
    if len(heights) == 1:
        total_height = heights[0]
    else:
        heights.sort()
        total_height = heights[-1] + heights[-2]

    max_height = max(max_height, total_height)

print(max_height / 2)
