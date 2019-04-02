#include "pq.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#define SWAP(a, b) { Element aux = (a); (a) = (b); (b) = aux; }

PriorityQueue pq_nou(int capacitate) {
    PriorityQueue pq;

    pq.elem = (Element*)malloc(sizeof(Element) * capacitate);
    pq.n = 0;
    pq.cap = capacitate;

    return pq;
}

void pq_free(PriorityQueue* pq) {
    free(pq->elem);
    pq->elem = NULL;
    pq->n = pq->cap = 0;
}

Element pq_min(const PriorityQueue* pq) {
    assert(pq->n != 0 && "Priority queue-ul este gol!");

    return pq->elem[0];
}

void pq_afisare(const PriorityQueue* pq) {
    for (int i = 0; i < pq->n; ++i) {
        printf("(P%d: %d) ", pq->elem[i].prio, pq->elem[i].val);
    }
    printf("\n");
}

void pq_sift_up(PriorityQueue* pq, int idx) {
    while (idx != 0) {
        int tata = (idx - 1) / 2;

        if (pq->elem[tata].prio > pq->elem[idx].prio) {
            SWAP(pq->elem[tata], pq->elem[idx]);
            idx = tata;
        } else {
            break;
        }
    }
}

void pq_insert(PriorityQueue* pq, int prio, int nr) {
    assert((pq->n != pq->cap) && "Priority queue plin");

    int idx = pq->n++;
    pq->elem[idx] = (Element){ prio, nr };

    while (idx != 0) {
        int tata = (idx - 1) / 2;

        if (pq->elem[tata].prio > pq->elem[idx].prio) {
            SWAP(pq->elem[tata], pq->elem[idx]);
            idx = tata;
        } else {
            break;
        }
    }
}

void pq_sift_down(PriorityQueue* pq, int idx) {
    int st = 2 * idx + 1, dr = 2 * idx + 2;

    int fiu = idx;

    if (st < pq->n && pq->elem[st].prio < pq->elem[fiu].prio) {
        fiu = st;
    }
    if (dr < pq->n && pq->elem[dr].prio < pq->elem[fiu].prio) {
        fiu = dr;
    }

    if (idx != fiu) {
        SWAP(pq->elem[idx], pq->elem[fiu]);
        pq_sift_down(pq, fiu);
    }
}

Element pq_pop(PriorityQueue* pq) {
    assert((pq->n != 0) && "Priority queue gol");

    Element ret = pq->elem[0];

    SWAP(pq->elem[0], pq->elem[pq->n - 1]);

    pq->n--;

    pq_sift_down(pq, 0);

    return ret;
}

int pq_cauta(const PriorityQueue* pq, int nr) {
    for (int i = 0; i < pq->n; ++i) {
        if (pq->elem[i].val == nr) {
            return i;
        }
    }

    return -1;
}
