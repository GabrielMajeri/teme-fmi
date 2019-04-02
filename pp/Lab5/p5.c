#include <stdlib.h>
#include <stdio.h>

int main() {
    FILE* fin = fopen("p5.in", "r");

    int n, m;
    fscanf(fin, "%d %d ", &n, &m);

    int* a = (int*)malloc(sizeof(int) * n * m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            fscanf(fin, "%d ", a + (i * m + j));
        }
    }

    int i = 0, j = 0;
    int k = 0, nr = 0;

    while (nr < n * m) {
        for (j = k; j < (m - k); ++j) {
            printf("%d ", a[i * m + j]);
            ++nr;
        }

        --j;

        if (nr >= n * m) {
            break;
        }

        for (i = k + 1; i < (n - k); ++i) {
            printf("%d ", a[i * m + j]);
            ++nr;
        }

        --i;

        if (nr >= n * m) {
            break;
        }

        for (j = (m - k - 2); j >= k; --j) {
            printf("%d ", a[i * m + j]);
            ++nr;
        }

        ++j;

        if (nr >= n * m) {
            break;
        }

        for (i = (n - k - 2); i > k; --i) {
            printf("%d ", a[i * m + j]);
            ++nr;
        }

        ++i;

        if (nr >= n * m) {
            break;
        }

        ++k;
    }

    printf("\n");

    free(a);
}
