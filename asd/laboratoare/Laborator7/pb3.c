#include "abc.h"
#include <stdio.h>

int main() {
    FILE* f = fopen("pb3.in", "r");

    int n;
    fscanf(f, "%d", &n);

    ABC* abc = abc_citire(f, n);

    fclose(f);

    abc_inordine(abc);
    printf("\n");
}
