#include <stdio.h>
#include <stdlib.h>

typedef struct Indexat Indexat;

struct Indexat {
    Indexat* st, * dr;
    int info;
    int marime_st;
};

Indexat* ind_nod_nou(int info) {
    Indexat* p = (Indexat*)malloc(sizeof(Indexat));

    p->st = p->dr = NULL;
    p->info = info;
    p->marime_st = 0;

    return p;
}

void ind_free(Indexat* t) {
    if (t == NULL) {
        return;
    }

    ind_free(t->st);
    ind_free(t->dr);

    free(t);
}

Indexat* ind_insereaza(Indexat* t, int info) {
    if (t == NULL) {
        return ind_nod_nou(info);
    }

    if (info < t->info) {
        t->st = ind_insereaza(t->st, info);
        t->marime_st++;
    } else if (t->info < info) {
        t->dr = ind_insereaza(t->dr, info);
    }

    return t;
}

Indexat* ind_citeste(FILE* f, int n) {
    Indexat* rad = NULL;

    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(f, "%d", &x);

        rad = ind_insereaza(rad, x);
    }

    return rad;
}

void ind_afisare(const Indexat* p) {
    if (p == NULL) {
        return;
    }

    ind_afisare(p->st);
    printf("(%d, i=%d) ", p->info, p->marime_st);
    ind_afisare(p->dr);
}

int ind_cautare(const Indexat* p, int k) {
    int marime_st = p->marime_st;

    if (k < marime_st) {
        return ind_cautare(p->st, k);
    } else if (k > marime_st) {
        return ind_cautare(p->dr, k - marime_st - 1);
    } else {
        return p->info;
    }
}

int main() {
    FILE* f = fopen("pb5.in", "r");

    int n;
    fscanf(f, "%d", &n);

    Indexat* idx = ind_citeste(f, n);

    int k;
    fscanf(f, "%d", &k);

    fclose(f);

    printf("Elementul care s-ar afla pe pozitia %d este %d\n",
        k, ind_cautare(idx, k));

    printf("Vectorul in inordine: ");
    ind_afisare(idx);
    printf("\n");

    ind_free(idx);
}
