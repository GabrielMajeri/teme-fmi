#ifndef PQ_H
#define PQ_H

typedef struct Nod Nod;

struct Nod {
    int info, prio;
    Nod* urm;
};

typedef struct {
    Nod* prim;
} PQ;

PQ pq_nou(void);
void pq_free(PQ* pq);

void pq_inserare(PQ* pq, int info, int prio);
int pq_extrage(PQ* pq, int* prio);

void pq_afisare(const PQ* pq);

#endif
