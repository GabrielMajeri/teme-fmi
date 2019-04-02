#include <stdlib.h>
#include <stdio.h>

int determinant(const int* a, int n) {
    if (n <= 0) {
        return 0;
    }

    if (n == 1) {
        return a[0];
    }

    if (n == 2) {
        int x = a[0];
        int y = a[1];
        int z = a[2];
        int w = a[3];

        return x * w - y * z;
    }

    const int m = n - 1;

    // dezvoltare dupa prima linie
    int semn = 1, det = 0;

    for (int j = 0; j < n; ++j) {
        int* minor = (int*)malloc(m * m * sizeof(int));

        int k = 0;
        for (int x = 1; x < n; ++x) {
            for (int y = 0; y < n; ++y) {
                if (j == y) {
                    continue;
                }

                minor[k++] = a[x * n + y];
            }
        }

        int det_minor = determinant(minor, m);

        free(minor);

        det += semn * a[j] * det_minor;

        semn *= -1;
    }

    return det;
}

int main() {
    FILE* fin = fopen("p8.in", "r");

    int n;
    fscanf(fin, "%d", &n);

    int* a = (int*)malloc(sizeof(int) * n * n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fscanf(fin, "%d", &a[i * n + j]);
        }
    }

    fclose(fin);

    int det = determinant(a, n);

    printf("%d\n", det);

    free(a);
}
