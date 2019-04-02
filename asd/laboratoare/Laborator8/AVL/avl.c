#include "avl.h"

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

AVL* avl_nou(int info) {
    AVL* p = (AVL*)malloc(sizeof(AVL));

    p->info = info;
    p->inaltime = 1;
    p->st = p->dr = NULL;

    return p;
}

void avl_free(AVL* avl) {
    if (avl == NULL) {
        return;
    }

    avl_free(avl->st);
    avl_free(avl->dr);

    free(avl);
}

// afisare in SRD recursiva
static void afisare(AVL* avl) {
    if (avl == NULL) {
        return;
    }

    afisare(avl->st);
    printf("%d ", avl->info);
    afisare(avl->dr);
}

void avl_afisare(AVL* avl) {
    afisare(avl);
    printf("\n");
}

int avl_inaltime(AVL* avl) {
    if (avl == NULL) {
        return 0;
    }
    return avl->inaltime;
}

int avl_balance(AVL* avl) {
    if (avl == NULL) {
        return 0;
    }

    return avl_inaltime(avl->st) - avl_inaltime(avl->dr);
}

static void update_inaltime(AVL* avl) {
    int h_st = avl_inaltime(avl->st);
    int h_dr = avl_inaltime(avl->dr);

    avl->inaltime = 1 + ((h_st > h_dr) ? h_st : h_dr);
}

AVL* avl_rotire_dreapta(AVL* radacina) {
    AVL* noua_radacina = radacina->st;
    AVL* nrad_dr = noua_radacina->dr;

    noua_radacina->dr = radacina;
    radacina->st = nrad_dr;

    update_inaltime(noua_radacina);
    update_inaltime(radacina);

    return noua_radacina;
}

AVL* avl_rotire_stanga(AVL* radacina) {
    AVL* noua_radacina = radacina->dr;
    AVL* nrad_st = noua_radacina->st;

    noua_radacina->st = radacina;
    radacina->dr = nrad_st;

    update_inaltime(radacina);
    update_inaltime(noua_radacina);

    return noua_radacina;
}

AVL* avl_rebalansare(AVL* rad, int elem) {
    // recalculam inaltimea
    update_inaltime(rad);

    int bal = avl_balance(rad);

    if (bal > 1) {
        // rotire dreapta
        if (elem < rad->st->info) {
            return avl_rotire_dreapta(rad);
        }
        // rotire stanga-dreapta
        if (rad->st->info < elem) {
            rad->st = avl_rotire_stanga(rad->st);
            return avl_rotire_dreapta(rad);
        }
    } else if (bal < -1) {
        // rotire stanga
        if (elem > rad->dr->info) {
            return avl_rotire_stanga(rad);
        }
        // rotire dreapta-stanga
        if (elem < rad->dr->info) {
            rad->dr = avl_rotire_dreapta(rad->dr);
            return avl_rotire_stanga(rad);
        }
    }

    return rad;
}

AVL* avl_inserare(AVL* rad, int elem) {
    // daca se construieste un subarbore nou
    if (rad == NULL) {
        return avl_nou(elem);
    }

    // incercam sa inseram in subarborele corect
    if (elem < rad->info) {
        rad->st = avl_inserare(rad->st, elem);
    } else if (rad->info < elem) {
        rad->dr = avl_inserare(rad->dr, elem);
    } else {
        return rad;
    }

    // rebalansare
    return avl_rebalansare(rad, elem);
}

AVL* avl_sterge(AVL* rad, int elem) {
    if (rad == NULL) {
        return NULL;
    }

    if (elem < rad->info) {
        rad->st = avl_sterge(rad->st, elem);
        return rad;
    } else if (rad->info < elem) {
        rad->dr = avl_sterge(rad->dr, elem);
        return rad;
    }

    // cazul cand trebuie sters `rad`

    // daca are cel putin un fiu NULL
    if (rad->st == NULL || rad->dr == NULL) {
        // care nod este nenul (daca este vreunul)
        AVL* nenul = rad->st ? rad->st : rad->dr;

        if (nenul) {
            *rad = *nenul;
            free(nenul);
        } else {
            // daca este frunza
            free(rad);
            return NULL;
        }
    } else {
        // radacina are ambii fii
        int maxim = avl_maxim(rad->st);
        rad->info = maxim;
        rad->st = avl_sterge(rad->st, maxim);
    }

    return avl_rebalansare(rad, elem);
}

bool avl_cauta(const AVL* rad, int elem) {
    if (rad == NULL) {
        return false;
    }

    if (elem < rad->info) {
        return avl_cauta(rad->st, elem);
    } else if (rad->info < elem) {
        return avl_cauta(rad->dr, elem);
    } else {
        return true;
    }
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))

int avl_maxim(const AVL* rad) {
    if (rad == NULL) {
        return INT_MIN;
    }

    int info = rad->info;
    int max_dreapta = avl_maxim(rad->dr);

    return MAX(info, max_dreapta);
}
