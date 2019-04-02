#include "deque.h"

#include <stdlib.h>

Deque deque_nou(void) {
    Deque d = { lista_nou() };

    return d;
}

void deque_free(Deque* d) {
    lista_free(&d->l);
}

void deque_push_front(Deque* d, int info) {
    lista_adauga_inceput(&d->l, info);
}

int deque_pop_front(Deque* d) {
    if (d->l.prim == NULL) {
        return -1;
    }

    int info = d->l.prim->info;
    lista_sterge_prim(&d->l);

    return info;
}

void deque_push_back(Deque* d, int info) {
    lista_adauga_sfarsit(&d->l, info);
}

int deque_pop_back(Deque* d) {
    if (d->l.ultim == NULL) {
        return -1;
    }

    int info = d->l.ultim->info;

    lista_sterge_ultim(&d->l);

    return info;
}
