#include <stdio.h>

#include "avl.h"

int main() {
    int v[] = { 68, 75, 84, 8, 9, 13, 54, 25, 98, 84 };
    int n = sizeof(v) / sizeof(v[0]);

    AVL* avl = NULL;

    for (int i = 0; i < n; ++i) {
        avl = avl_inserare(avl, v[i]);
    }

    printf("Arbore: ");
    avl_afisare(avl);

    printf("Inaltime arbore: %d\n", avl_inaltime(avl));

    int cheie = 13;
    printf("Caut %d in arbore: %d\n", cheie, avl_cauta(avl, cheie));

    cheie = 13;
    avl = avl_sterge(avl, cheie);
    printf("AVL dupa ce am sters %d: ", cheie);
    avl_afisare(avl);

    printf("\n");

    avl_free(avl);
}
