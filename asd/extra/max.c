#include <stdio.h>

/// Minim dintre `a` si `b`
int min(int a, int b) {
    return a > b ? b : a;
}

/// Maxim dintre `a` si `b`
int max(int a, int b) {
    return a > b ? a : b;
}

/// Maxim dintre `a`, `b` si `c`
int max3(int a, int b, int c) {
    return max(max(a, b), c);
}

typedef struct {
    int minim, maxim, max_diff;
} Rezultat;

/// Calculeaza minimul vectorului, maximul vectorului,
/// si diferenta maxima a doua elemente v[i] si v[j], cu i < j
Rezultat max_diff(const int v[], int st, int dr) {
    Rezultat r;

    // conditie de oprire
    if (st + 1 >= dr) {
        r.minim = min(v[st], v[dr]);
        r.maxim = max(v[st], v[dr]);
        r.max_diff = v[dr] - v[st];

        return r;
    }

    int mid = st + (dr - st) / 2;

    Rezultat a = max_diff(v, st, mid - 1);
    Rezultat b = max_diff(v, mid, dr);

    r.minim = min(a.minim, b.minim);
    r.maxim = max(a.maxim, b.maxim);
    r.max_diff = max3(a.max_diff, b.max_diff, b.maxim - a.minim);

    return r;
}

int main() {
    const int v[] = { 0, -1, 3, 2, 1, 2, 7, 5 };
    int n = sizeof(v) / sizeof(int);

    Rezultat r = max_diff(v, 0, n - 1);

    printf("%d\n", r.max_diff);
}
