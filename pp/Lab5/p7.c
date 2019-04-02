#include <stdlib.h>
#include <stdio.h>

void roteste_dreapta(const int* a, int* b, int n) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            int elem = a[i * n + j];

            b[j * n + (n - i - 1)] = elem;
        }
    }
}

int main() {
    FILE* fin = fopen("p7.in", "r");

    int n;
    fscanf(fin, "%d", &n);

    int* a = (int*)malloc(sizeof(int) * n * n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fscanf(fin, "%d", &a[i * n + j]);
        }
    }

    fclose(fin);

    int* b = (int*)malloc(sizeof(int) * n * n);

    roteste_dreapta(a, b, n);

    FILE* fout = fopen("p7.out", "w");

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fprintf(fout, "%d ", b[i * n + j]);
        }
        fprintf(fout, "\n");
    }

    fclose(fout);

    free(a);
    free(b);
}
