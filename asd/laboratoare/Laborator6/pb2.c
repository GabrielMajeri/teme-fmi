#include "pq/pq.h"

int main() {
    PQ pq = pq_nou();

    pq_inserare(&pq, 1, 2);
    pq_inserare(&pq, 2, -1);
    pq_inserare(&pq, 3, 10);
    pq_inserare(&pq, 5, 3);
    pq_inserare(&pq, 6, 4);

    pq_afisare(&pq);

    pq_free(&pq);
}
