#pragma once

#include <stdio.h>

typedef struct {
    int dim;
    int** vecini;
} GrafLV;

GrafLV graf_nou(int n);
void graf_free(GrafLV g);

GrafLV graf_citeste(FILE* fin);

void graf_adauga_vecin(GrafLV g, int i, int vecin);
