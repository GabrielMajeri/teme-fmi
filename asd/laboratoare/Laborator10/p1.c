#include <stdio.h>
#include "graf_ma.h"

int main(void) {
    FILE* fin = fopen("p1.in", "r");

    GrafMA g = graf_citeste(fin);

    fclose(fin);

    int m = graf_nr_muchii(g);
    printf("Numar de muchii: %d\n", m);

    graf_print_grad_max(g);

    graf_free(g);
}
