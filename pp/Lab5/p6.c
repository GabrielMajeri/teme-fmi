#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int div_propriu(int n) {
    if (n <= 2) {
        return n;
    }

    int d = 2;

    while (d <= n / 2) {
        if (n % d == 0) {
            return d;
        }
        ++d;
    }

    return n;
}

int urmator(int* c) {
    static bool add_div;
    static int dp;

    if (add_div) {
        add_div = false;
        return dp;
    }

    int n = *c;
    dp = div_propriu(n);

    if (dp != n) {
        add_div = true;
    }

    *c = n + 1;

    return n;
}

int main() {
    FILE* fin = fopen("p6.in", "r");

    int n, m;
    fscanf(fin, "%d %d ", &n, &m);

    fclose(fin);

    int* a = (int*)malloc(sizeof(int) * n * m);

    int i = 0, j = 0;
    int k = 0, nr = 0;
    int curent = 1;

    while (nr < n * m) {
        for (j = k; j < (m - k); ++j) {
            a[i * m + j] = urmator(&curent);
            ++nr;
        }

        --j;

        if (nr >= n * m) {
            break;
        }

        for (i = k + 1; i < (n - k); ++i) {
            a[i * m + j] = urmator(&curent);
            ++nr;
        }

        --i;

        if (nr >= n * m) {
            break;
        }

        for (j = (m - k - 2); j >= k; --j) {
            a[i * m + j] = urmator(&curent);
            ++nr;
        }

        ++j;

        if (nr >= n * m) {
            break;
        }

        for (i = (n - k - 2); i > k; --i) {
            a[i * m + j] = urmator(&curent);
            ++nr;
        }

        ++i;

        if (nr >= n * m) {
            break;
        }

        ++k;
    }

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            printf("%d ", a[i * m + j]);
        }
        printf("\n");
    }

    printf("\n");

    free(a);
}
