from math import acos, pi
from typing import NamedTuple


class Point(NamedTuple):
    x: float
    y: float

    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)

    def __repr__(self):
        return f'{self.x} {self.y}'

    def abs(self):
        return dot_product(self, self) ** 0.5


def approx_eq(num, target):
    return abs(target - num) < 0.001


def dot_product(u, v):
    return u.x * v.x + u.y * v.y


def cos_angle(u, v):
    return dot_product(u, v) / (u.abs() * v.abs())


def angle(u, v):
    return acos(cos_angle(u, v))


def cross_product(u, v):
    return u.x * v.y - u.y * v.x


def orientation_test(a, b, c):
    return cross_product(b - a, c - a)


def left_turn(a, b, c):
    return orientation_test(a, b, c) > 0


def right_turn(a, b, c):
    return orientation_test(a, b, c) < 0


with open('convex.txt') as fin:
    points = []
    for _ in range(4):
        x, y = next(fin).split()
        points.append(Point(float(x), float(y)))

convex = (
    (left_turn(points[0], points[1], points[2]) and
     left_turn(points[1], points[2], points[3]) and
     left_turn(points[2], points[3], points[0]) and
     left_turn(points[3], points[0], points[1])) or
    (right_turn(points[0], points[1], points[2]) and
     right_turn(points[1], points[2], points[3]) and
     right_turn(points[2], points[3], points[0]) and
     right_turn(points[3], points[0], points[1]))
)

if convex:
    print("Convex")
    a41 = points[3] - points[0]
    a43 = points[3] - points[2]
    a21 = points[1] - points[0]
    a23 = points[1] - points[2]

    u1 = angle(a41, a43)
    u2 = angle(a21, a23)

    s = u1 + u2

    if approx_eq(s, pi):
        print("A4 este pe cerc")
    elif s < pi:
        print("A4 este în exteriorul cercului")
    else:
        print("A4 este în interiorul cercului")
else:
    print("Concav")
