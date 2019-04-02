#include <stdlib.h>
#include <stdio.h>

int main() {
    FILE* fin = fopen("p4.in", "r");

    int m, n;
    fscanf(fin, "%d %d ", &m, &n);

    int* a = (int*)malloc(sizeof(int) * m * n);

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            int x;
            fscanf(fin, "%d ", &x);

            a[i * n + j] = x;
        }
    }

    fclose(fin);

    int* b = (int*)malloc(sizeof(int) * n * m);

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            b[j * m + i] = a[i * n + j];
        }
    }

    free(a);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            printf("%d ", b[i * m + j]);
        }
        printf("\n");
    }

    free(b);
}
