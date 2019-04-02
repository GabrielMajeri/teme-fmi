#include "pq.h"
#include <stdlib.h>
#include <stdio.h>

PQ pq_nou() {
    PQ pq = { NULL };
    return pq;
}

static Nod* nod_nou(int info, int prio) {
    Nod* nod = (Nod*)malloc(sizeof(Nod));

    nod->info = info;
    nod->prio = prio;
    nod->urm = NULL;

    return nod;
}

void pq_inserare(PQ* pq, int info, int prio) {
    Nod* nod = nod_nou(info, prio);

    if (pq->prim == NULL) {
        pq->prim = nod;
        return;
    }

    Nod* p = pq->prim;

    if (prio < p->prio) {
        nod->urm = p;
        pq->prim = nod;
        return;
    }

    while (p->urm && p->urm->prio < prio) {
        p = p->urm;
    }

    nod->urm = p->urm;
    p->urm = nod;
}

int pq_extrage(PQ* pq, int* prio) {
    if (pq->prim == NULL) {
        return -1;
    }

    Nod* nod = pq->prim;
    pq->prim = nod->urm;

    int info = nod->info;

    if (prio) {
        *prio = nod->prio;
    }

    free(nod);

    return info;
}

void pq_free(PQ* pq) {
    Nod* nod = pq->prim;

    while (nod) {
        Nod* urm = nod->urm;
        free(nod);
        nod = urm;
    }

    pq->prim = NULL;
}

void pq_afisare(const PQ* pq) {
    for (Nod* p = pq->prim; p != NULL; p = p->urm) {
        printf("%d ", p->info);
    }

    printf("\n");
}
