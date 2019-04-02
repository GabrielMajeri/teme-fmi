#include "abc.h"
#include <stdlib.h>

ABC* abc_nou(int info) {
    ABC* p = (ABC*)malloc(sizeof(ABC));

    p->info = info;
    p->st = p->dr = NULL;

    return p;
}

void abc_free(ABC* abc) {
    if (abc == NULL) {
        return;
    }

    abc_free(abc->st);
    abc_free(abc->dr);

    free(abc);
}

ABC* abc_insert(ABC* t, int info) {
    if (t == NULL) {
        return abc_nou(info);
    }

    if (info < t->info) {
        t->st = abc_insert(t->st, info);
    } else if (t->info < info) {
        t->dr = abc_insert(t->dr, info);
    }

    return t;
}

bool abc_search(const ABC* t, int info) {
    if (t == NULL) {
        return false;
    }

    if (info < t->info) {
        return abc_search(t->st, info);
    } else if (t->info < info) {
        return abc_search(t->dr, info);
    }

    return true;
}

int abc_find_max(const ABC* t) {
    if (t == NULL) {
        return -1;
    }

    if (t->dr == NULL) {
        return t->info;
    } else {
        return abc_find_max(t->dr);
    }
}

ABC* abc_delete(ABC* t, int info) {
    if (t == NULL) {
        return NULL;
    }

    if (info < t->info) {
        t->st = abc_delete(t->st, info);
        return t;
    } else if (t->info < info) {
        t->dr = abc_delete(t->dr, info);
        return t;
    }

    bool are_st = t->st != NULL,
        are_dr = t->dr != NULL;

    if (!are_st && !are_dr) {
        free(t);
        return NULL;
    }

    if (!(are_st && are_dr)) {
        if (are_st) {
            t->info = t->st->info;
            t->st = abc_delete(t->st, info);
            return t->st;
        } else {
            t->info = t->dr->info;
            t->dr = abc_delete(t->dr, info);
            return t->dr;
        }
    }

    int pred = abc_find_max(t->st);

    t->st = abc_delete(t->st, pred);
    t->info = pred;

    return t;
}

ABC* abc_citire(FILE* f, int n) {
    ABC* abc = NULL;

    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(f, "%d", &x);

        abc = abc_insert(abc, x);
    }

    return abc;
}

void abc_preordine(const ABC* t) {
    if (t == NULL) {
        return;
    }

    printf("%d ", t->info);

    abc_preordine(t->st);
    abc_preordine(t->dr);
}

void abc_inordine(const ABC* t) {
    if (t == NULL) {
        return;
    }

    abc_inordine(t->st);
    printf("%d ", t->info);
    abc_inordine(t->dr);
}
