#include <stdio.h>
#include "pq.h"

int main() {
    PriorityQueue pq = pq_nou(100);

    pq_insert(&pq, 5, 2);
    pq_insert(&pq, 7, 36);
    pq_insert(&pq, 1, -5);
    pq_insert(&pq, 6, 9);
    pq_insert(&pq, 5, 17);

    while (pq.n != 0) {
        Element e = pq_pop(&pq);
        printf("Element %d cu prioritatea %d\n", e.val, e.prio);
    }

    pq_free(&pq);
}
