#ifndef COADA_H
#define COADA_H

#include <stdbool.h>

typedef struct nod nod;

struct nod {
    int nr;
    nod* urm;
};

typedef struct {
    nod* prim, * ultim;
} coada;

coada coada_nou(void);
void coada_free(coada* c);

void coada_push(coada* c, int nr);
int coada_pop(coada* c);

bool coada_empty(const coada* c);
int coada_peek(const coada* c);

int coada_search(const coada* c, int a);
void coada_afisare(const coada* c);

#endif // COADA_H
