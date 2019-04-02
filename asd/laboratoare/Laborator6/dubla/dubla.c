#include "dubla.h"
#include <stdlib.h>
#include <stdio.h>

Nod* nod_nou(int val) {
    Nod* p = (Nod*)malloc(sizeof(Nod));

    p->st = p->dr = NULL;
    p->info = val;

    return p;
}

Lista lista_nou() {
    Lista l = {
        .prim = NULL,
        .ultim = NULL,
    };

    return l;
}

void lista_free(Lista* l) {
    Nod* p = l->prim;

    while (p) {
        Nod* dr = p->dr;
        free(p);
        p = dr;
    }

    l->prim = l->ultim = NULL;
}

void lista_adauga_inceput(Lista* l, int info) {
    Nod* prim = l->prim;

    Nod* p = nod_nou(info);

    if (prim) {
        p->dr = prim;
        prim->st = p;
    } else {
        l->ultim = p;
    }

    l->prim = p;
}

void lista_adauga_sfarsit(Lista* l, int info) {
    Nod* ultim = l->ultim;

    Nod* p = nod_nou(info);

    if (ultim) {
        ultim->dr = p;
        p->st = ultim;
    } else {
        l->prim = p;
    }

    l->ultim = p;
}

void lista_insereaza_dupa(Lista* l, Nod* p, int info) {
    Nod* dr = p->dr;

    Nod* nou = nod_nou(info);

    nou->st = p;
    nou->dr = dr;

    p->dr = nou;

    if (p == l->ultim) {
        l->ultim = nou;
    }
}

void lista_insereaza_poz(Lista* l, int k, int info) {
    Nod* p = l->prim;
    while (p && k > 0) {
        p = p->dr;
        --k;
    }

    if (p == NULL) {
        printf("Inserare dupa pozitie inexistenta\n");
        return;
    }

    lista_insereaza_dupa(l, p, info);
}

void lista_afisare(const Lista* l) {
    for (Nod* p = l->prim; p != NULL; p = p->dr) {
        printf("%d ", p->info);
    }
    printf("\n");
}

void lista_afisare_invers(const Lista* l) {
    for (Nod* p = l->ultim; p != NULL; p = p->st) {
        printf("%d ", p->info);
    }
    printf("\n");
}

void lista_sterge_prim(Lista* l) {
    Nod* prim = l->prim;

    if (!prim) {
        return;
    }

    Nod* dr = prim->dr;

    l->prim = dr;

    if (dr == NULL) {
        l->ultim = NULL;
    }

    free(prim);
}

void lista_sterge_ultim(Lista* l) {
    Nod* ultim = l->ultim;

    if (!ultim) {
        return;
    }

    Nod* st = ultim->st;

    l->ultim = st;

    if (st == NULL) {
        l->prim = NULL;
    }

    free(ultim);
}

void lista_sterge_poz(Lista* l, int k) {
    if (k == 0) {
        return lista_sterge_prim(l);
    }

    Nod* p = l->prim;
    while (p && k > 0) {
        p = p->dr;
        --k;
    }

    if (p == NULL) {
        printf("Stergere dupa pozitie inexistenta");
        return;
    }

    if (p->dr == NULL) {
        return lista_sterge_ultim(l);
    }

    p->st->dr = p->dr;
    p->dr->st = p->st;

    free(p);
}

void lista_sterge_val(Lista* l, int info) {
    Nod* prim = l->prim;

    if (!prim) {
        return;
    }

    if (prim->info == info) {
        return lista_sterge_prim(l);
    }

    Nod* p = prim->dr;

    while (p && p->info != info) {
        p = p->dr;
    }

    Nod* st = p->st;
    Nod* dr = p->dr;

    if (dr == NULL) {
        return lista_sterge_ultim(l);
    }

    st->dr = dr;
    dr->st = st;

    free(p);
}
