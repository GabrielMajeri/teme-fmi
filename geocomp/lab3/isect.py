from typing import NamedTuple


class Point(NamedTuple):
    x: float
    y: float

    def __repr__(self):
        return f'({self.x}, {self.y})'


def approx_eq(num, target):
    return abs(target - num) < 0.001


def line_equation(p1, p2):
    dy = p2.y - p1.y
    dx = p2.x - p1.x

    # Vertical line
    if dx == 0:
        return 1, 0, -p1.x

    slope = dy / dx

    # y = slope * x + intercept
    intercept = p1.y - slope * p1.x

    return -slope, 1, -intercept


def determinant(a, b, c, d):
    return a * d - b * c


def check_point_in_segment(p, a, b):
    min_x, max_x = min(a.x, b.x), max(a.x, b.x)
    min_y, max_y = min(a.y, b.y), max(a.y, b.y)

    return min_x <= p.x <= max_x and min_y <= p.y <= max_y


def find_intersection(p1, p2, p3, p4):
    a1, b1, c1 = line_equation(p1, p2)
    a2, b2, c2 = line_equation(p3, p4)

    delta = determinant(a1, b1, a2, b2)

    if not approx_eq(delta, 0):
        x = determinant(-c1, b1, -c2, b2) / delta
        y = determinant(a1, -c1, a2, -c2) / delta
        p = Point(x, y)

        if check_point_in_segment(p, p1, p2) and check_point_in_segment(p, p3, p4):
            return p,
        else:
            return tuple()
    else:
        d1 = determinant(a1, -c1, a2, -c2)
        d2 = determinant(b1, -c1, b2, -c2)

        if approx_eq(d1, 0.0) and approx_eq(d2, 0.0):
            # rang 1

            if (check_point_in_segment(p3, p1, p2)
                    and check_point_in_segment(p2, p3, p4)):
                return p1, p2

        # rang 2
        return tuple()


fin = open('isect.txt')
lines = (next(fin).split() for _ in range(4))
points = [Point(float(x), float(y)) for x, y in lines]
isect = find_intersection(*points)

if len(isect) == 0:
    print('Intersecția este mulțimea vidă')
elif len(isect) == 1:
    print('Intersecția este punctul', isect[0])
else:
    print(f'Intersecția este segmentul [{isect[0]}, {isect[1]}]')
