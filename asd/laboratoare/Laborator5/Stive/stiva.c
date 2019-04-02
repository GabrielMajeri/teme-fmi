#include "stiva.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

stiva stiva_nou() {
    stiva st = { NULL };
    return st;
}

void stiva_free(stiva* st) {
    nod* p = st->varf;
    while (p) {
        nod* urm = p->urm;
        free(p);
        p = urm;
    }
    st->varf = NULL;
}

void stiva_push(stiva* st, int val) {
    nod* p = (nod*)malloc(sizeof(nod));
    p->nr = val;
    p->urm = st->varf;

    st->varf = p;
}

int stiva_pop(stiva* st) {
    int val = stiva_peek(st);

    nod* p = st->varf;
    st->varf = p->urm;
    free(p);

    return val;
}

bool stiva_empty(const stiva* st) {
    return st->varf == NULL;
}

int stiva_peek(const stiva* st) {
    assert(!stiva_empty(st));

    return st->varf->nr;
}

int stiva_search(const stiva* st, int a) {
    int k = 0;
    for (nod* p = st->varf; p; p = p->urm, ++k) {
        if (p->nr == a) {
            return k;
        }
    }

    return -1;
}

void stiva_afisare(const stiva* st) {
    for (nod* p = st->varf; p; p = p->urm) {
        printf("%d ", p->nr);
    }

    printf("\n");
}
