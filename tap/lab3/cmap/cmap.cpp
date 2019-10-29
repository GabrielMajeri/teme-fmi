#include <bits/stdc++.h>
using namespace std;

struct Point {
    int x, y;
};

istream& operator>>(istream& is, Point& pt) {
    return is >> pt.x >> pt.y;
}

ostream& operator<<(ostream& os, const Point& pt) {
    return os << "(" << pt.x << ", " << pt.y << ")";
}

double distance_square(Point a, Point b) {
    double dx = double(a.x) - double(b.x),
        dy = double(a.y) - double(b.y);

    return dx * dx + dy * dy;
}

double min_distance(size_t num_pts, const Point* xs, const Point* ys) {
    if (num_pts <= 1) {
        return INFINITY;
    }
    if (num_pts == 2) {
        return distance_square(xs[0], xs[1]);
    }
    if (num_pts == 3) {
        double a = distance_square(xs[0], xs[1]),
            b = distance_square(xs[0], xs[2]),
            c = distance_square(xs[1], xs[2]);
        return min(a, min(b, c));
    }

    int mid = num_pts / 2;
    int mid_x = xs[mid].x;

    const Point* left_xs = xs;
    const Point* right_xs = xs + mid;
    vector<Point> left_ys, right_ys;
    left_ys.reserve(mid);
    right_ys.reserve(mid);

    for (size_t i = 0; i < num_pts; ++i) {
        if (ys[i].x < mid_x) {
            left_ys.push_back(ys[i]);
        } else {
            right_ys.push_back(ys[i]);
        }
    }

    double min_left = min_distance(mid, left_xs, left_ys.data()),
        min_right = min_distance(mid, right_xs, right_ys.data());

    double d = min(min_left, min_right);

    vector<Point> closer;
    closer.reserve(num_pts);
    for (size_t i = 0; i < num_pts; ++i) {
        double dx = abs(ys[i].x - mid_x);
        if (dx <= d) {
            closer.push_back(ys[i]);
        }
    }

    if (closer.size() > 0) {
        size_t i = 0;
        while (i < closer.size() - 1) {
            size_t j = i + 1;
            while (j < closer.size() && (j - i) <= 8) {
                d = min(d, distance_square(closer[i], closer[j]));
                ++j;
            }
            ++i;
        }
    }

    return d;
}

int main() {
    ifstream in("cmap.in");

    size_t num_pts;
    in >> num_pts;

    vector<Point> pts(num_pts);
    for (size_t i = 0; i < num_pts; ++i) {
        in >> pts[i];
    }

    vector<Point> xs = pts, ys = pts;

    sort(xs.begin(), xs.end(), [] (const Point& a, const Point& b) { return a.x < b.x; });
    sort(ys.begin(), ys.end(), [] (const Point& a, const Point& b) { return a.y < b.y; });

    ofstream out("cmap.out");
    double d_squared = min_distance(num_pts, xs.data(), ys.data());
    out << fixed << setprecision(6) << sqrt(d_squared) << '\n';
}
