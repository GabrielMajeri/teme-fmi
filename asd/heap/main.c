#include <stdio.h>
#include "heap.h"

int main() {
    Heap h = heap_nou(1024);

    heap_insert(&h, 15);
    heap_insert(&h, 7);
    heap_insert(&h, 10);
    heap_insert(&h, 11);
    heap_insert(&h, 9);
    heap_insert(&h, 5);
    heap_insert(&h, 8);
    heap_insert(&h, 1);

    heap_afisare(&h);

    int min = heap_pop(&h);

    printf("min = %d\n", min);
    heap_afisare(&h);

    heap_free(&h);
}
