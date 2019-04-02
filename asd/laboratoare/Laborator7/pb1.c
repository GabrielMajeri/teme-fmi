#include <stdio.h>
#include <stdlib.h>

void adauga_frunza(int* v, int* n, int val) {
    v[*n] = val;
    *n = *n + 1;
}

void preordine(const int* v, int n, int i) {
    if (i >= n) {
        return;
    }

    printf("%d ", v[i]);

    int st = 2 * i + 1, dr = 2 * i + 2;

    preordine(v, n, st);
    preordine(v, n, dr);
}

void inordine(const int* v, int n, int i) {
    if (i >= n) {
        return;
    }

    int st = 2 * i + 1, dr = 2 * i + 2;

    inordine(v, n, st);

    printf("%d ", v[i]);

    inordine(v, n, dr);
}

void postordine(const int* v, int n, int i) {
    if (i >= n) {
        return;
    }

    int st = 2 * i + 1, dr = 2 * i + 2;

    postordine(v, n, st);
    postordine(v, n, dr);

    printf("%d ", v[i]);
}

int main() {
    FILE* f = fopen("pb1.in", "r");

    int nr;
    fscanf(f, "%d", &nr);

    int* v = (int*)calloc(nr, sizeof(int));
    int n = 0;

    for (int i = 0; i < nr; ++i) {
        int x;
        fscanf(f, "%d", &x);
        adauga_frunza(v, &n, x);
    }

    fclose(f);

    printf("Preordine: ");
    preordine(v, n, 0);
    printf("\n");

    printf("Inordine: ");
    inordine(v, n, 0);
    printf("\n");

    printf("Postordine: ");
    postordine(v, n, 0);
    printf("\n");

    free(v);
}
