#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stiva.h"

void teste_stiva() {
    stiva st = stiva_nou();

    stiva_push(&st, 5);
    stiva_push(&st, 7);
    stiva_push(&st, -3);
    stiva_push(&st, 4);
    stiva_push(&st, 6);

    printf("Stiva: ");
    stiva_afisare(&st);

    int x = -3;
    printf("Caut %d: distanta %d\n", x, stiva_search(&st, x));

    printf("Primele 3 elemente din varf: ");
    printf("%d ", stiva_pop(&st));
    printf("%d ", stiva_pop(&st));
    printf("%d ", stiva_pop(&st));
    printf("\n");

    printf("Adaug un 10\n");
    stiva_push(&st, 10);

    printf("Primele 3 elemente din varf: ");
    printf("%d ", stiva_pop(&st));
    printf("%d ", stiva_pop(&st));
    printf("%d ", stiva_pop(&st));

    stiva_free(&st);

    printf("\n");
}

bool aparitii_egale(int n, int nr_a, int nr_b) {
    // conditie de oprire
    if (n < 0) {
        return nr_a == nr_b;
    }

    char x;
    scanf("%c", &x);

    bool recursiv = aparitii_egale(n - 1, nr_a + (x == 'a'), nr_b + (x == 'b'));

    return recursiv;
}

void problema2() {
    printf("Problema 2\n n = ");

    int n;
    scanf("%d", &n);

    printf("%d\n", aparitii_egale(n, 0, 0));
}

void problema3() {
    printf("Problema 3:\n");

    char s[100];
    scanf("%80s", s);

    int n = strlen(s);

    stiva st = stiva_nou();

    for (int i = 0; i < n; ++i) {
        if (s[i] == '(') {
            stiva_push(&st, 1);
        } else {
            if (stiva_empty(&st)) {
                printf("Paranteza care nu poate fi inchisa la pozitia %d\n", i);
                stiva_free(&st);
                return;
            } else {
                stiva_pop(&st);
            }
        }
    }

    if (!stiva_empty(&st)) {
        printf("Paranteze ramase deschise la sfarsit\n");
        return;
    }

    stiva_free(&st);

    printf("Sir parantezat corect\n");
}

void problema4() {
    printf("Problema 4:\n");

    int n;
    scanf("%d", &n);

    stiva st = stiva_nou();

    for (int i = 0; i < n; ++i) {
        int x;
        scanf("%d", &x);

        if (!stiva_empty(&st) && stiva_peek(&st) == x) {
            stiva_pop(&st);
        } else {
            stiva_push(&st, x);
        }
    }

    if (!stiva_empty(&st)) {
        printf("Configuratie invalida\n");

        stiva_free(&st);
        return;
    }

    stiva_free(&st);

    printf("Configuratie valida\n");
}

int main() {
    teste_stiva();

    //problema2();

    //problema3();

    //problema4();
}
