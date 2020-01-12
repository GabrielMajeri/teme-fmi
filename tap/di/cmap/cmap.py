from collections import namedtuple
from math import inf, sqrt

Point = namedtuple('Point', ('x', 'y'))


def read_point(f):
    x, y = map(int, next(f).split())
    return Point(x, y)


def distance_square(a, b):
    return (a.x - b.x) ** 2 + (a.y - b.y) ** 2


def min_distance(xs, ys):
    if len(xs) <= 1:
        return inf
    if len(xs) == 2:
        return distance_square(xs[0], xs[1])
    if len(xs) == 3:
        return min(distance_square(xs[0], xs[1]),
                   distance_square(xs[0], xs[2]),
                   distance_square(xs[1], xs[2]))

    # print('Sortate după X:', *xs)
    # print('Sortate după Y:', *ys)

    mid = len(xs) // 2
    mid_x = xs[mid].x
    # print(f'Despart punctele prin dreapta x={mid_x}')

    xs_left, xs_right = xs[:mid], xs[mid:]
    ys_left, ys_right = [], []
    for pt in ys:
        if pt.x < mid_x:
            ys_left.append(pt)
        else:
            ys_right.append(pt)

    # print(*xs_left, '<', *xs_right)
    # print(*ys_left, '<', *ys_right)

    min_left = min_distance(xs_left, ys_left)
    min_right = min_distance(xs_right, ys_right)

    d = min(min_left, min_right)

    closer = [pt for pt in ys if (pt.x - mid_x) ** 2 < d]
    # print(*closer)
    i = 0
    while i < len(closer) - 1:
        j = i + 1
        while j < len(closer) and j - i <= 8:
            d = min(d, distance_square(closer[i], closer[j]))
            j += 1
        i += 1

    return min(min_left, min_right)


with open('cmap.in', 'r') as fin:
    num_points = int(next(fin))
    points = [read_point(fin) for _ in range(num_points)]

sorted_by_x = sorted(points, key=lambda pt: pt.x)
sorted_by_y = sorted(points, key=lambda pt: pt.y)

min_dist = min_distance(sorted_by_x, sorted_by_y)
min_dist = sqrt(min_dist)

with open('cmap.out', 'w') as fout:
    print(min_dist, file=fout)
