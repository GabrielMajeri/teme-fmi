#pragma once

#include <stdbool.h>
#include <stdio.h>

typedef struct ABC ABC;

struct ABC {
    int info;
    ABC* st, * dr;
};

ABC* abc_nou(int info);
void abc_free(ABC* abc);

ABC* abc_insert(ABC* t, int info);
bool abc_search(const ABC* t, int info);

int abc_find_max(const ABC* t);
ABC* abc_delete(ABC* t, int info);

ABC* abc_citire(FILE* f, int n);

void abc_preordine(const ABC* t);
void abc_inordine(const ABC* t);
