#ifndef STIVA_H
#define STIVA_H

#include <stdbool.h>

typedef struct nod nod;

struct nod {
    int nr;
    nod* urm;
};

typedef struct {
    nod* varf;
} stiva;

stiva stiva_nou(void);
void stiva_free(stiva* st);

void stiva_push(stiva* st, int val);
int stiva_pop(stiva* st);

bool stiva_empty(const stiva* st);
int stiva_peek(const stiva* st);

int stiva_search(const stiva* st, int a);

void stiva_afisare(const stiva* st);

#endif // STIVA_H
