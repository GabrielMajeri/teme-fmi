#include <stdio.h>
#include <stdlib.h>

int** mat_alloc(int n, int m) {
    int** a = (int**)calloc(n, sizeof(int*));

    for (int i = 0; i < n; ++i) {
        a[i] = (int*)calloc(m, sizeof(int));
    }

    return a;
}

void mat_free(int** a, int n) {
    for (int i = 0; i < n; ++i) {
        free(a[i]);
    }

    free(a);
}

int main() {
    FILE* fin = fopen("p1.in", "r");

    int n;
    fscanf(fin, "%d", &n);

    int** a = mat_alloc(n, n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fscanf(fin, "%d", &a[i][j]);
        }
    }

    fclose(fin);

    int** b = mat_alloc(n, n - 1);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            b[i][j] = a[i][j];
        }
        for (int j = i + 1; j < n; ++j) {
            b[i][j - 1] = a[i][j];
        }
    }

    mat_free(a, n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n - 1; ++j) {
            printf("%d ", b[i][j]);
        }
        printf("\n");
    }

    mat_free(b, n - 1);
}
