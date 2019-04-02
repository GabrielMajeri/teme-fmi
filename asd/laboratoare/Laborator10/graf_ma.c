#include "graf_ma.h"
#include <stdlib.h>
#include <stdio.h>

GrafMA graf_nou(int n) {
    GrafMA g;

    g.dim = n;
    g.adiacenta = (bool**)calloc(n, sizeof(bool*));

    for (int i = 0; i < n; ++i) {
        g.adiacenta[i] = (bool*)calloc(n, sizeof(bool));
    }

    return g;
}

void graf_free(GrafMA g) {
    for (int i = 0; i < g.dim; ++i) {
        free(g.adiacenta[i]);
        g.adiacenta[i] = NULL;
    }
    free(g.adiacenta);
}

GrafMA graf_citeste(FILE* fin) {
    int n;
    fscanf(fin, "%d", &n);

    GrafMA g = graf_nou(n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            int x = 0;

            fscanf(fin, "%d", &x);

            g.adiacenta[i][j] = x;
        }
    }

    return g;
}

int graf_grad(GrafMA g, int i) {
    int grad = 0;
    for (int j = 0; j < g.dim; ++j) {
        grad += g.adiacenta[i][j];
    }
    return grad;
}

int graf_nr_muchii(GrafMA g) {
    int suma = 0;

    for (int i = 0; i < g.dim; ++i) {
        suma += graf_grad(g, i);
    }

    return suma;
}

int max(int a, int b) {
    return a > b ? a : b;
}

void graf_print_grad_max(GrafMA g) {
    int m = 0;

    for (int i = 0; i < g.dim; ++i) {
        m = max(m, graf_grad(g, i));
    }

    printf("Noduri de grad maxim (%d): ", m);

    for (int i = 0; i < g.dim; ++i) {
        if (graf_grad(g, i) == m) {
            printf("%d ", i);
        }
    }

    printf("\n");
}
