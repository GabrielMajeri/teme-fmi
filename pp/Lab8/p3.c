#include <stdlib.h>
#include <stdio.h>

int** mat_alloc(int n) {
    int** a = (int**)malloc(n * sizeof(int*));

    for (int i = 0; i < n; ++i) {
        a[i] = (int*)malloc(n * sizeof(int));
    }

    return a;
}

void mat_read(FILE* f, int** a, int n) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fscanf(f, "%d", &a[i][j]);
        }
    }
}

void mat_print(FILE* f, int** a, int n) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fprintf(f, "%d ", a[i][j]);
        }
        fprintf(f, "\n");
    }
}

int mat_centru(int** a, int n) {
    int mid = n / 2;

    return a[mid][mid];
}

void mat_diag_principala(int** a, int n) {
    for (int i = 0; i < n; ++i) {
        printf("%d ", a[i][i]);
    }
    printf("\n");
}

void mat_diag_secundara(int** a, int n) {
    for (int i = 0; i < n; ++i) {
        printf("%d ", a[i][n - i - 1]);
    }
    printf("\n");
}

void mat_interschimba(int** a, int l1, int l2) {
    int* tmp = a[l1];
    a[l1] = a[l2];
    a[l2] = tmp;
}

void mat_free(int** a, int n) {
    for (int i = 0; i < n; ++i) {
        free(a[i]);
    }

    free(a);
}

int main() {
    FILE* f = fopen("p3.in", "r");

    int n;
    fscanf(f, "%d", &n);

    int** a = mat_alloc(n);

    mat_read(f, a, n);

    fclose(f);

    printf("Matricea este: \n");
    mat_print(stdout, a, n);

    printf("Intersectia diagonalelor: ");
    printf("%d\n", mat_centru(a, n));

    printf("Diagonala principala: ");
    mat_diag_principala(a, n);

    printf("Diagonala secundara: ");
    mat_diag_secundara(a, n);

    printf("Prima si ultima linie interschimbata: \n");
    mat_interschimba(a, 0, n - 1);

    mat_print(stdout, a, n);

    mat_free(a, n);
}
