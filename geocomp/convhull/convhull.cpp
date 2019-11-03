#include <bits/stdc++.h>
using namespace std;

ifstream in("infasuratoare.in");
ofstream out("infasuratoare.out");

struct Point {
    double x, y;

    bool operator<(Point rhs) const {
        return make_tuple(x, y) < make_tuple(rhs.x, rhs.y);
    }

    friend istream& operator>>(istream& is, Point& pt) {
        return is >> pt.x >> pt.y;
    }

    friend ostream& operator<<(ostream& os, const Point& pt) {
        return os << pt.x << ' ' << pt.y;
    }

    Point operator-(Point b) const {
        return { x - b.x, y - b.y };
    }
};

double cross_product(Point u, Point v) {
    return u.x * v.y - u.y * v.x;
}

double orientation_test(Point a, Point b, Point c) {
    return cross_product(b - a, c - a);
}

bool right_turn(Point a, Point b, Point c) {
    return orientation_test(a, b, c) < 0;
}

vector<Point> compute_hull(const vector<Point>& points) {
    vector<Point> hull = { points[0], points[1] };

    int i = 2;
    while (i < points.size()) {
        hull.push_back(points[i]);
        while (hull.size() >= 3
            && !right_turn(hull.rbegin()[2], hull.rbegin()[1], hull.rbegin()[0]))
        {
            hull.erase(hull.begin() + hull.size() - 2);
        }
        ++i;
    }

    return hull;
}

template <typename T>
void print_vector(const vector<T>& v) {
    for (auto elem : v) {
        out << elem << '\n';
    }
}

int main() {
    int n;
    in >> n;

    vector<Point> points(n);
    for (int i = 0; i < n; ++i) {
        in >> points[i];
    }

    sort(points.begin(), points.end());

    auto lower_hull = compute_hull(points);
    lower_hull.erase(lower_hull.begin());

    reverse(points.begin(), points.end());

    auto upper_hull = compute_hull(points);
    upper_hull.erase(upper_hull.begin());

    vector<Point> hull = lower_hull;
    hull.insert(hull.end(), upper_hull.cbegin(), upper_hull.cend());

    out << hull.size() << '\n';
    print_vector(hull);
}
