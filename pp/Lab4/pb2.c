#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef struct {
    char adresa[101];
    float suprafata;
    char tip[31];
    unsigned int nr_camere;
    union {
        struct {
            bool balcon: 1;
        } garsoniera;

        struct {
            char decomandat;
        } apartament;

        union {
            char sir[20];
        } casa;
    } date;
} locuinta;

void citeste_locuinta(locuinta* l) {
    scanf("%100s", l->adresa);
    scanf("%f", &l->suprafata);
    scanf("%30s", l->tip);
    scanf("%u", &l->nr_camere);

    if (strcmp(l->tip, "garsoniera") == 0) {
        int x;
        scanf("%d", &x);
        l->date.garsoniera.balcon = x;
    } else if (strcmp(l->tip, "casa") == 0) {
        scanf("%20c", l->date.casa.sir);
    } else {
        char ch;
        scanf("%c", &ch);
        l->date.apartament.decomandat = ch;
    }
}

int main() {
    int n;
    scanf("%d", &n);

    locuinta* l = (locuinta*)calloc(n, sizeof(locuinta));

    locuinta* max = NULL;

    for (int i = 0; i < n; ++i) {
        citeste_locuinta(l + i);

        if (strcmp(l[i].tip, "garsoniera") == 0
            && l[i].date.garsoniera.balcon) {
            if (!max) {
                max = l + i;
            } else {
                if (max->suprafata < l[i].suprafata) {
                    max = l + i;
                }
            }
        }
    }

    if (max == NULL) {
        printf("Nu exista garsoniere cu balcon\n");
    } else {
        printf("%s\n", max->adresa);
    }

    free(l);
}
