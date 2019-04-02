#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "graf_lv.h"

void parcurge_bfs(GrafLV g, bool* vizitat, int i) {
    if (vizitat[i]) {
        return;
    }

    vizitat[i] = true;

    int* vecini = g.vecini[i] + 1;
    int nr = g.vecini[i][0];

    for (int j = 0; j < nr; ++j) {
        if (vizitat[vecini[j]] == false) {
            printf("%d ", vecini[j] + 1);
        }
    }

    for (int j = 0; j < nr; ++j) {
        parcurge_bfs(g, vizitat, vecini[j]);
    }
}

int main() {
    FILE* fin = fopen("p3.in", "r");

    GrafLV g = graf_citeste(fin);

    int k;
    fscanf(fin, "%d", &k);
    k -= 1;

    fclose(fin);

    bool* vizitat = calloc(g.dim, sizeof(bool));

    parcurge_bfs(g, vizitat, k);
    printf("\n");

    free(vizitat);

    graf_free(g);
}
