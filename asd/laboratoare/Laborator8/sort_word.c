#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct ABC ABC;

struct ABC {
    char* cuvant;
    ABC* st, * dr;
};

ABC* abc_nod_nou(const char* s) {
    ABC* rad = (ABC*)malloc(sizeof(ABC));

    rad->cuvant = (char*)malloc(strlen(s) + 1);
    strcpy(rad->cuvant, s);

    rad->st = rad->dr = NULL;

    return rad;
}

void abc_free(ABC* abc) {
    if (abc == NULL) {
        return;
    }

    abc_free(abc->st);
    abc_free(abc->dr);

    free(abc);
}

ABC* abc_inserare(ABC* rad, const char* elem) {
    if (rad == NULL) {
        return abc_nod_nou(elem);
    }

    int cmp = strcmp(elem, rad->cuvant);

    if (cmp < 0) {
        rad->st = abc_inserare(rad->st, elem);
    } else if (cmp > 0) {
        rad->dr = abc_inserare(rad->dr, elem);
    }

    return rad;
}

void abc_srd_rev(const ABC* rad) {
    if (rad == NULL) {
        return;
    }

    abc_srd_rev(rad->dr);
    printf("%s ", rad->cuvant);
    abc_srd_rev(rad->st);
}

int main() {
    const char* cuvinte[] = {
        "casa", "zoo", "alfabet", "cuvant", "telescop", "c++",
    };

    int lg = sizeof(cuvinte) / sizeof(cuvinte[0]);

    ABC* rad = NULL;

    for (int i = 0; i < lg; ++i) {
        rad = abc_inserare(rad, cuvinte[i]);
    }

    abc_srd_rev(rad);
    printf("\n");
}
