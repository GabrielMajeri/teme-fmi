#include <stdlib.h>
#include <stdio.h>
#include "pq.h"

int main() {
    FILE* fin = fopen("la_coada.in", "r");

    int n;
    fscanf(fin, "%d", &n);

    PriorityQueue pq = pq_nou(n);

    for (int i = 1; i <= n; ++i) {
        pq_insert(&pq, i, i);
    }

    int contor = n + 1;

    int k;
    fscanf(fin, "%d", &k);

    for (int i = 0; i < k; ++i) {
        int operatie;
        fscanf(fin, "%d", &operatie);

        switch (operatie) {
            case 1:
            {
                pq_pop(&pq);
                break;
            }
            case 2:
            {
                int prio = contor++;
                pq_insert(&pq, prio, prio);
                break;
            }
            case 3:
            {
                int x;
                fscanf(fin, "%d", &x);

                int idx = pq_cauta(&pq, x);

                pq.elem[idx].prio = pq.elem[0].prio - 1;
                pq_sift_up(&pq, idx);

                break;
            }
            default:
                printf("Operatie necunoscuta\n");
                return 1;
        }
    }

    fclose(fin);
    FILE* fout = fopen("la_coada.out", "w");

    int lg = pq.n;
    fprintf(fout, "%d\n", lg);

    for (int i = 0; i < lg; ++i) {
        fprintf(fout, "%d ", pq_pop(&pq).val);
    }

    pq_free(&pq);

    fclose(fout);
}
