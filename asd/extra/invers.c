#include <stdio.h>
#include <stdlib.h>

typedef struct Nod Nod;

struct Nod {
    Nod* urm;
    int info;
};

Nod* nod_nou(int val) {
    Nod* p = (Nod*)malloc(sizeof(Nod));

    p->info = val;
    p->urm = NULL;

    return p;
}

void sterge_lista(Nod* lista) {
    if (!lista) {
        return;
    }

    sterge_lista(lista->urm);

    free(lista);
}

void afisare(Nod* lista) {
    for (Nod* p = lista; p; p = p->urm) {
        printf("%d ", p->info);
    }
    printf("\n");
}

void inversare(Nod** lista) {
    Nod* st = NULL, * c = *lista;

    while (c) {
        Nod* urm = c->urm;
        c->urm = st;
        st = c;
        c = urm;
    }

    *lista = st;
}

int main() {
    Nod* l = nod_nou(10);
    l->urm = nod_nou(-3);
    l->urm->urm = nod_nou(15);
    l->urm->urm->urm = nod_nou(23);

    afisare(l);

    inversare(&l);

    afisare(l);

    sterge_lista(l);
}
