from collections import namedtuple

Point = namedtuple('Point',
                   ('x', 'y', 'z'))


def subtract(a, b):
    return Point(b.x - a.x, b.y - a.y, b.z - a.z)


def solve(a, b, c):
    if a == b:
        print('Coliniare')
        print('b = 1 * a + 0 * c')
        return

    ab = subtract(a, b)
    ac = subtract(a, c)

    r = 0
    if ab.x != 0:
        rx = ac.x / ab.x
        r = rx
    if ab.y != 0:
        ry = ac.y / ab.y
        r = ry
    if ab.z != 0:
        rz = ac.z / ab.z
        r = rz

    if (ab.x * r == ac.x) and (ab.y * r == ac.y) and (ab.z * r == ac.z):
        print('Coliniare')
        print(f'c = (1 - {r}) * a + {r} * b')
    else:
        print('Necoliniare')


fin = open('coliniar.txt')
lines = map(str.strip, fin)


def read_point():
    x, y, z = map(int, next(lines).split())
    return Point(x, y, z)


solve(read_point(), read_point(), read_point())
