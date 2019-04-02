#include <stdio.h>
#include <stdlib.h>

int max(int a, int b) {
    return a > b ? a : b;
}

int maxim(const int v[], int n) {
    if (n <= 1) {
        return v[0];
    }

    int mijl = n / 2;

    return max(maxim(v, mijl), maxim(v + mijl, mijl));
}

int main() {
    int v[] = { 2, 3, 1, 5, 7, 1, 2, 4, 0 };
    int n = sizeof(v) / sizeof(v[0]);

    printf("%d\n", maxim(v, n));
}
