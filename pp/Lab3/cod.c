#include "cod.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char upper_to_lower_case(char ch) {
    if (isupper(ch)) {
        return ch + ('a' - 'A');
    } else {
        return ch;
    }
}

int parse_int() {
    int n;
    scanf("%d", &n);

    return n;
}

char parse_operator() {
    char op;

    // sar peste spatii
    while ((op = getchar()) == ' ')
        ;

    return op;
}

int eval_expr(int a, char op, int b) {
    switch (op) {
    case '+':
        return a + b;
    case '-':
        return a - b;
    case '*':
        return a * b;
    case '/':
        if (b == 0) {
            printf("Impartire la 0\n");
            return -1;
        }
        return a / b;
    default:
        printf("Operator necunoscut: %c\n", op);
        return -1;
    }
}

static int compara_candidat(const candidat* a, const candidat* b) {
    return strcmp(a->nume, b->nume);
}

void citeste_candidat(candidat* c) {
    scanf("%d\n", &c->nr_legitimatie);

    scanf("%64s", c->nume);

    scanf("%lf %lf %lf",
        &c->nota_mate, &c->nota_info, &c->nota_bac);

    c->medie = MEDIE_ADMITERE(c);
    c->admis = c->medie > MEDIE_MINIMA;
}

void insereaza_candidat(candidat v[], int* n, const candidat* c) {
    if (compara_candidat(c, &v[0]) < 0) {
        for (int i = *n; i >= 1; --i) {
            v[i] = v[i - 1];
        }

        ++*n;
        v[0] = *c;

        return;
    }

    for (int i = 0; i < *n - 1; ++i) {
        if (compara_candidat(c, &v[i]) > 0) {
            break;
        }
    }

    v[*n] = *c;
    ++*n;
}

static int compara_candidat_medie(const void* a, const void* b) {
    candidat** pa = (candidat**)a;
    candidat** pb = (candidat**)b;

    double ma = (*pa)->medie;
    double mb = (*pb)->medie;

    if (ma < mb) {
        return -1;
    } else if (ma > mb) {
        return 1;
    } else {
        return 0;
    }
}

void completeaza_buget(candidat v[], int n) {
    candidat** vp = (candidat**)calloc(n, sizeof(candidat*));

    int admisi = 0;
    for (int i = 0; i < n; ++i) {
        vp[admisi++] = &v[i];
    }

    qsort(vp, admisi, sizeof(candidat*), compara_candidat_medie);

    int i;
    for (i = 0; i < 3 * admisi / 4; ++i) {
        vp[i]->buget = true;
    }

    for (; i < admisi; ++i) {
        vp[i]->buget = false;
    }
}

void afisare_elevi(const candidat* v, int n) {
    for (int i = 0; i < n; ++i) {
        const candidat* c = v + i;
        printf("Elev #%d: %s\n", c->nr_legitimatie, c->nume);
        printf("Medii = %lf %lf %lf => %lf\n",
               c->nota_mate, c->nota_info, c->nota_bac,
               c->medie);
        printf("Admis: %d\n", c->admis);
        printf("\n");
    }
}

void meniu_elevi(const candidat* v, int n) {
    printf("Introduceti comanda:\n");
    printf("(a)fisare\n");
    printf("(b)uget\n");
    printf("(t)axa\n");
    printf("(r)espinsi\n");

    char comanda = getchar();
    switch (comanda) {
    case 'a':
        afisare_elevi(v, n);
        break;

    case 'b':

        break;
    case 't':

        break;
    case 'r':

        break;

    default:
        printf("Comanda necunoscuta\n");
        return;
    }
}
