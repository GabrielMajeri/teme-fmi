from collections import namedtuple

Point = namedtuple('Point', ('x', 'y'))

EPSILON = 0.001


def almost_equal(a, b):
    return abs(a - b) < EPSILON


def tri_area(a, b, c):
    det = ((b.x * c.y - c.x * b.y) -
            (a.x * c.y - c.x * a.y) +
            (a.x * b.y - b.x * a.y))
    return 0.5 * abs(det)


def colinear(a, b, c):
    return almost_equal(tri_area(a, b, c), 0)


def side(p, a, b):
    return (p.x - a.x) * (b.y - a.y) - (p.y - a.y) * (b.x - a.x) > 0


def point_inside_triangle(p, a, b, c):
    sum_area = tri_area(p, a, b) + tri_area(p, b, c) + tri_area(p, c, a)
    big_area = tri_area(a, b, c)
    return sum_area == big_area


def solve(points):
    if (colinear(points[0], points[1], points[2])
            and colinear(points[1], points[2], points[3])):
        points.sort()
        return 'segment', [points[0], points[3]], [points[1], points[2]]

    if point_inside_triangle(points[0], points[1], points[2], points[3]):
        return 'triunghi', [points[1], points[2]], [points[0]]

    if point_inside_triangle(points[1], points[0], points[2], points[3]):
        return 'triunghi', [points[0], points[2], points[3]], [points[1]]

    if point_inside_triangle(points[2], points[0], points[1], points[3]):
        return 'triunghi', [points[0], points[1], points[3]], [points[2]]

    if point_inside_triangle(points[3], points[0], points[1], points[2]):
        return 'triunghi', [points[0], points[1], points[2]], [points[3]]

    if side(points[2], points[0], points[1]) != side(points[3], points[0], points[1]):
        return 'patrulater', [points[0], points[1]], [points[2], points[3]]
    if side(points[1], points[0], points[2]) != side(points[3], points[0], points[2]):
        return 'patrulater', [points[0], points[2]], [points[1], points[3]]

    return 'patrulater', [points[0], points[3]], [points[1], points[2]]


fin = open('convex.txt')
points = []

for _ in range(4):
    x, y = map(float, next(fin).split())
    points.append(Point(x, y))

shape, I, J = solve(points)

print(f'Acoperirea convexa este un {shape}')
print('I:', *I)
print('J:', *J)
