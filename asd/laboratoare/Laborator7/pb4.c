#include <stdio.h>
#include <stdlib.h>
#include "abc.h"

void afisare_interval(const ABC* t, int st, int dr) {
    if (t == NULL) {
        return;
    }

    int info = t->info;

    if (st <= info && info <= dr) {
        printf("%d ", info);
    }

    if (dr < info) {
        afisare_interval(t->st, st, dr);
    } else if (info < st) {
        afisare_interval(t->dr, st, dr);
    } else {
        afisare_interval(t->st, st, dr);
        afisare_interval(t->dr, st, dr);
    }
}

int main() {
    FILE* f = fopen("pb4.in", "r");

    int n;
    fscanf(f, "%d", &n);

    ABC* abc = abc_citire(f, n);

    int st, dr;
    fscanf(f, "%d %d", &st, &dr);

    fclose(f);

    printf("Valori din intervalul [%d, %d]: ", st, dr);
    afisare_interval(abc, st, dr);
    printf("\n");

    abc_free(abc);
}
