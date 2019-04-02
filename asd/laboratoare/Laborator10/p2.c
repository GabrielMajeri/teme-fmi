#include <stdio.h>
#include <stdlib.h>
#include "graf_lv.h"

void marcheaza(GrafLV g, int* componente, int i, int marker) {
    if (componente[i] != 0) {
        return;
    }

    componente[i] = marker;

    int* vecini = g.vecini[i] + 1;
    int nr = g.vecini[i][0];

    for (int i = 0; i < nr; ++i) {
        marcheaza(g, componente, vecini[i], marker);
    }
}

void componente_conexe(GrafLV g) {
    int* componente = (int*)calloc(g.dim, sizeof(int));
    int nr_comp = 0;

    for (int i = 0; i < g.dim; ++i) {
        if (componente[i] != 0) {
            continue;
        }

        int marker = ++nr_comp;

        marcheaza(g, componente, i, marker);
    }

    for (int i = 1; i <= nr_comp; ++i) {
        printf("Componenta conexa nr. %d\n", i);
        for (int j = 0; j < g.dim; ++j) {
            if (componente[j] == i) {
                printf("%d ", j + 1);
            }
        }
        printf("\n");
    }

    free(componente);
}

int main() {
    FILE* fin = fopen("p2.in", "r");

    GrafLV g = graf_citeste(fin);

    fclose(fin);

    componente_conexe(g);

    graf_free(g);
}
