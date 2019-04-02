#include "abc.h"
#include <stdio.h>

int main() {
    FILE* f = fopen("pb2.in", "r");

    int n;
    fscanf(f, "%d", &n);

    ABC* abc = abc_citire(f, n);

    fclose(f);

    int cautat = 4;
    printf("%d se afla in arbore: %d\n", cautat, abc_search(abc, cautat));

    printf("Maximul este %d\n", abc_find_max(abc));

    printf("Inordine: ");
    abc_inordine(abc);
    printf("\n");

    int nod = 4;
    printf("Sterg nodul %d\n", nod);
    abc = abc_delete(abc, nod);

    printf("Inordine: ");
    abc_inordine(abc);
    printf("\n");

    abc_free(abc);
}
