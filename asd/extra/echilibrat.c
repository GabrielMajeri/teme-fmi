#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

typedef struct Arbore Arbore;

struct Arbore {
    Arbore* st, * dr;
    int info;
};

Arbore* nod_nou(int val) {
    Arbore* a = (Arbore*)malloc(sizeof(Arbore));

    a->st = a->dr = NULL;
    a->info = val;

    return a;
}

void nod_free(Arbore* a) {
    if (!a) {
        return;
    }

    nod_free(a->st);
    nod_free(a->dr);

    free(a);
}

int adancime(Arbore* a) {
    if(!a) {
        return 0;
    }

    return 1 + adancime(a->st) + adancime(a->dr);
}

bool echilibrat(Arbore* a) {
    int st = adancime(a->st);
    int dr = adancime(a->dr);

    return abs(dr - st) <= 1;
}

int main() {
    Arbore* rad = nod_nou(4);

    rad->st = nod_nou(2);
    rad->dr = nod_nou(5);
    rad->dr->st = nod_nou(1);
    rad->dr->dr = nod_nou(6);

    printf("Echilibrat: %d\n", echilibrat(rad));

    nod_free(rad);
}
