#include "graf_lv.h"

#include <stdlib.h>
#include <stdio.h>

GrafLV graf_nou(int n) {
    GrafLV g;

    g.dim = n;
    g.vecini = (int**)calloc(n, sizeof(int*));

    for (int i = 0; i < n; ++i) {
        g.vecini[i] = (int*)calloc(n + 1, sizeof(int));
    }

    return g;
}

void graf_free(GrafLV g) {
    for (int i = 0; i < g.dim; ++i) {
        free(g.vecini[i]);
    }

    free(g.vecini);
}

GrafLV graf_citeste(FILE* fin) {
    int n;
    fscanf(fin, "%d", &n);

    GrafLV g = graf_nou(n);

    int m;
    fscanf(fin, "%d", &m);

    for (int i = 0; i < m; ++i) {
        int st, dr;
        fscanf(fin, "%d %d", &st, &dr);

        st -= 1;
        dr -= 1;

        graf_adauga_vecin(g, st, dr);
        graf_adauga_vecin(g, dr, st);
    }

    return g;
}

void graf_adauga_vecin(GrafLV g, int i, int vecin) {
    int* vecini = g.vecini[i] + 1;
    int* nr = g.vecini[i];

    vecini[(*nr)++] = vecin;
}
