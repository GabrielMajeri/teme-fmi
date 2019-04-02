#include <stdio.h>

#include "avl.h"

int main() {
    int v[] = { 6, 5, 7, 11, 2, 4, 3, 12, 9, 4, 8, 14, 13, 12 };
    int n = sizeof(v) / sizeof(v[0]);

    AVL* avl = NULL;

    for (int i = 0; i < n; ++i) {
        avl = avl_inserare(avl, v[i]);
    }

    avl_afisare(avl);

    printf("Inaltime arbore: %d\n", avl_inaltime(avl));

    avl_free(avl);
}
