#include "coada.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

coada coada_nou() {
    coada c = { NULL };

    return c;
}

void coada_free(coada* c) {
    nod* p = c->prim;

    while (p) {
        nod* urm = p->urm;
        free(p);
        p = urm;
    }

    c->prim = c->ultim = NULL;
}

void coada_push(coada* c, int nr) {
    nod* p = (nod*)malloc(sizeof(nod));
    p->nr = nr;
    p->urm = NULL;

    if (c->prim) {
        c->ultim->urm = p;
        c->ultim = p;
    } else {
        c->prim = c->ultim = p;
    }
}

int coada_pop(coada* c) {
    assert(!coada_empty(c));

    int val = coada_peek(c);

    nod* nou_prim = c->prim->urm;
    free(c->prim);
    c->prim = nou_prim;

    return val;
}

bool coada_empty(const coada* c) {
    return c->prim == NULL;
}

int coada_peek(const coada* c) {
    return c->prim->nr;
}

int coada_search(const coada* c, int a) {
    if (coada_empty(c)) {
        return -1;
    }

    int poz = 0;

    for (nod* p = c->prim; p; p = p->urm) {
        if (p->nr == a) {
            return poz;
        }
        ++poz;
    }

    return -1;
}

void coada_afisare(const coada* c) {
    for (nod* p = c->prim; p; p = p->urm) {
        printf("%d ", p->nr);
    }
}
