#pragma once

typedef struct {
    int* val;
    int n, cap;
} Heap;

Heap heap_nou(int capacitate);
void heap_free(Heap* h);

int heap_min(const Heap* h);
void heap_afisare(const Heap* h);

void heap_insert(Heap* h, int nr);
int heap_pop(Heap* h);
