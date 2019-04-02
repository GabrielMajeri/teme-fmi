#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "graf_ma.h"

void dfs(GrafMA g, bool* vizitat, int i) {
    if (vizitat[i]) {
        return;
    }

    vizitat[i] = true;

    printf("%d ", i + 1);

    for (int j = 0; j < g.dim; ++j) {
        if (g.adiacenta[i][j]) {
            dfs(g, vizitat, j);
        }
    }
}

int main() {
    FILE* fin = fopen("p4.in", "r");

    GrafMA g = graf_citeste(fin);

    int k;
    fscanf(fin, "%d", &k);
    --k;

    fclose(fin);

    bool* vizitat = (bool*)calloc(g.dim, sizeof(bool));

    dfs(g, vizitat, k);
    printf("\n");

    free(vizitat);
}
