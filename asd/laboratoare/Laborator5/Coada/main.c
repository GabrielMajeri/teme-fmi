#include <stdio.h>
#include <stdlib.h>
#include "coada.h"

void teste_coada() {
    coada c = coada_nou();

    coada_push(&c, 15);
    coada_push(&c, 4);
    coada_push(&c, 7);
    coada_push(&c, -3);

    printf("\n%d se afla la distanta %d\n", 7, coada_search(&c, 7));

    printf("%d ", coada_pop(&c));
    printf("%d ", coada_pop(&c));
    printf("%d ", coada_pop(&c));
    printf("%d ", coada_pop(&c));

    coada_free(&c);

    printf("\n");
}

int a[100][100];
int b[100][100];

void problema6() {
    printf("Problema 6:\n");

    FILE* f = fopen("imagine.in", "r");

    int m;
    fscanf(f, "%d", &m);

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < m; ++j) {
            int x;
            fscanf(f, "%d", &x);
            a[i][j] = x;
        }
    }

    fclose(f);

    int et = 1;

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < m; ++j) {
            if (a[i][j] == 0 || b[i][j] != 0) {
                continue;
            }

            coada x = coada_nou(), y = coada_nou();

            coada_push(&x, i);
            coada_push(&y, j);

            while (!coada_empty(&x)) {
                int cx = coada_pop(&x);
                int cy = coada_pop(&y);

                if (a[cx][cy] == 0 || b[cx][cy] != 0) {
                    continue;
                }

                b[cx][cy] = et;

                if (cx > 0) {
                    coada_push(&x, cx - 1);
                    coada_push(&y, cy);
                }
                if (cy > 0) {
                    coada_push(&x, cx);
                    coada_push(&y, cy - 1);
                }
                if (cx < m - 1) {
                    coada_push(&x, cx + 1);
                    coada_push(&y, cy);
                }
                if (cy < m - 1) {
                    coada_push(&x, cx);
                    coada_push(&y, cy + 1);
                }
            }

            ++et;

            coada_free(&x);
            coada_free(&y);
        }
    }

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < m; ++j) {
            printf("%d ", b[i][j]);
        }
        printf("\n");
    }
}

void problema7() {
    FILE* f = fopen("depo.in", "r");

    int n, k;
    fscanf(f, "%d %d", &n, &k);

    coada* depozit = (coada*)malloc(sizeof(coada) * k);
    int* maxime = (int*)malloc(sizeof(int) * k);

    for (int i = 0; i < k; ++i) {
        depozit[i] = coada_nou();
        maxime[i] = -1;
    }

    for (int i = 0; i < n; ++i) {
        int nr;
        fscanf(f, "%d", &nr);

        int ins_j = -1;
        for (int j = 0; j < k; ++j) {
            if (nr > maxime[j]) {
                ins_j = j;
                break;
            }
        }

        if (ins_j == -1) {
            printf("Nu se pot sorta vagoanele\n");
            return;
        } else {
            printf("Mut vagonul %d pe linia %d\n", nr, ins_j);
            maxime[ins_j] = nr;
            coada_push(depozit + ins_j, nr);
        }
    }

    printf("Toate vagoanele sunt pe linii de depozitare:\n");

    for (int i = 0; i < k; ++i) {
        printf("%d = ", i);
        coada_afisare(depozit + i);
        printf("\n");
    }

    for (int i = 0; i < n; ++i) {
        int min = 9999, min_j = 0;
        for (int j = 0; j < k; ++j) {
            if (coada_empty(depozit + j)) {
                continue;
            }

            int nr = coada_peek(depozit + j);

            if (nr < min) {
                min_j = j;
                min = nr;
            }
        }
        printf("Extrag %d de pe linia %d\n", coada_pop(depozit + min_j), min_j);
    }

    for (int i = 0; i < k; ++i) {
        coada_free(&depozit[i]);
    }

    free(maxime);
    free(depozit);
}

int main() {
    //teste_coada();

    //problema6();
    problema7();
}
