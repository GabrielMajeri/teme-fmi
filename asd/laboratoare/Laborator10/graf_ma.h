#pragma once

#include <stdbool.h>
#include <stdio.h>

typedef struct {
    int dim;
    bool** adiacenta;
} GrafMA;

GrafMA graf_nou(int n);
void graf_free(GrafMA g);

GrafMA graf_citeste(FILE* fin);

int graf_grad(GrafMA g, int i);
int graf_nr_muchii(GrafMA g);
void graf_print_grad_max(GrafMA g);
