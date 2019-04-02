#include <stdlib.h>
#include <stdio.h>
#include "dubla/dubla.h"

Lista lista_marcaj_nou() {
    Lista l = lista_nou();

    lista_adauga_inceput(&l, 0);

    l.prim->st = l.prim->dr = l.prim;

    return l;
}

void lista_marcaj_free(Lista* l) {
    l->prim->st->dr = NULL;

    lista_free(l);
}

void lista_marcaj_inserare(Lista* l, int info) {
    lista_adauga_sfarsit(l, info);

    l->ultim->dr = l->prim;
}

void lista_marcaj_stergere(Lista* l, int val) {
    for (Nod* p = l->prim->dr; p != l->prim; p = p->dr) {
        if (p->info == val) {
            Nod* st = p->st, *dr = p->dr;
            st->dr = dr;
            dr->st = st;
            free(p);

            break;
        }
    }

    l->ultim->dr = l->prim;
}

void lista_marcaj_afisare(const Lista* l) {
    if (!l->prim->dr) {
        return;
    }

    for (Nod* p = l->prim->dr; p != l->prim; p = p->dr) {
        printf("%d ", p->info);
    }

    printf("\n");
}


void lista_marcaj_push(Lista* l, int info) {
    Nod* p = nod_nou(info);
    p->st = l->prim;
    p->dr = l->prim->dr;

    l->prim->dr->st = p;
    l->prim->dr = p;

    if (p->dr == l->prim) {
        l->ultim = p;
        l->prim->st = p;
    }

    l->ultim->dr = l->prim;
}

int lista_marcaj_pop(Lista* l) {
    Nod* st = l->prim->st;
    Nod* nou_ultim = st->st;
    int info = st->info;

    l->ultim = l->prim->st = nou_ultim;

    free(st);

    return info;
}

int main() {
    printf("Problema 4\n");

    Lista l = lista_marcaj_nou();

    lista_marcaj_inserare(&l, 23);
    lista_marcaj_inserare(&l, 14);
    lista_marcaj_inserare(&l, 7);
    lista_marcaj_afisare(&l);

    lista_marcaj_stergere(&l, 14);
    lista_marcaj_afisare(&l);

    lista_marcaj_free(&l);

    printf("Problema 5\n");

    l = lista_marcaj_nou();

    lista_marcaj_push(&l, 1);
    lista_marcaj_push(&l, 2);
    lista_marcaj_push(&l, 3);

    printf("%d\n", lista_marcaj_pop(&l));
    printf("%d\n", lista_marcaj_pop(&l));
    printf("%d\n", lista_marcaj_pop(&l));
}
