class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    @staticmethod
    def read(f):
        x, y = map(float, next(f).split())
        return Point(x, y)

    def __lt__(self, other):
        return (self.x, self.y) < (other.x, other.y)

    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)

    def __repr__(self):
        return '{} {}'.format(self.x, self.y)


def cross_product(u, v):
    return u.x * v.y - u.y * v.x


def orientation_test(a, b, c):
    return cross_product(b - a, c - a)


def right_turn(a, b, c):
    return orientation_test(a, b, c) < 0


def compute_hull():
    hull = [points[0], points[1]]

    i = 2
    while i < len(points):
        hull.append(points[i])
        while len(hull) >= 3 and not right_turn(hull[-3], hull[-2], hull[-1]):
            hull.pop(-2)
        i += 1

    return hull


fin = open('infasuratoare.in')

n = int(next(fin))

points = [Point.read(fin) for _ in range(n)]

points.sort()

lower_hull = compute_hull()

points.reverse()
upper_hull = compute_hull()

hull = lower_hull[1:] + upper_hull[1:]

with open('infasuratoare.out', 'w') as fout:
    print(len(hull), *hull, sep='\n', file=fout)
