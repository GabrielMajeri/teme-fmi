#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int** a;
    int n;
} matrix;

matrix mat_create(void) {
    matrix m;

    m.a = (int**)malloc(0);
    m.n = 0;

    return m;
}

int* mat_add_line(matrix* m, int len) {
    m->a = (int**)realloc(m->a, (m->n + 1) * sizeof(int*));

    m->a[m->n] = (int*)calloc(len + 1, sizeof(int));
    m->a[m->n][0] = len;

    return m->a[m->n++];
}

void mat_free(matrix* m) {
    free(m->a);
    m->a = NULL;

    m->n = 0;
}

matrix mat_read(const char* path) {
    FILE* f = fopen(path, "r");

    matrix m = mat_create();

    while (true) {
        int n;
        fscanf(f, "%d", &n);

        if (feof(f)) {
            break;
        }

        int* linie = mat_add_line(&m, n);

        for (int i = 1; i <= n; ++i) {
            fscanf(f, "%d", &linie[i]);
        }
    }

    fclose(f);

    return m;
}

void mat_print(matrix m) {
    for (int i = 0; i < m.n; ++i) {
        int* linie = m.a[i];
        for (int j = 1; j <= linie[0]; ++j) {
            printf("%d ", linie[j]);
        }
        printf("\n");
    }
}

int cmp_int(const void* a, const void* b) {
    const int* x = (const int*)a;
    const int* y = (const int*)b;

    return *x - *y;
}

void mat_sort(matrix m) {
    for (int i = 0; i < m.n; ++i) {
        qsort(m.a[i] + 1, m.a[i][0], sizeof(int), cmp_int);
    }
}

int main() {
    matrix m = mat_read("p3.in");

    mat_print(m);

    mat_free(&m);
}
