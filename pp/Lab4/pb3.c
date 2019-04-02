#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    unsigned char varsta: 6;
    unsigned char norma: 1;
    char cnp[13];
    char nume[30];
} angajat;

void citire_angajat(angajat* a) {
    int varsta, norma;
    scanf("%d", &varsta);
    a->varsta = varsta;

    scanf("%30s", a->nume);

    scanf("%d", &norma);
    a->norma = norma;

    scanf("%13s", a->cnp);
}

bool este_barbat(const angajat* a) {
    return ((a->cnp[0] - '0') % 2) == 1;
}

int main() {
    printf("Dimensiune structura: %lu octeti\n",
        sizeof(angajat));

    int n;
    scanf("%d", &n);

    angajat* a = (angajat*)calloc(n, sizeof(angajat));

    for (int i = 0; i < n; ++i) {
        citire_angajat(a + i);
    }

    for (int i = 0; i < n; ++i) {
        const int masca = (1 << 5) - 1;
        const bool tanar = (a[i].varsta & ~masca) == 0;

        if (este_barbat(a + i) && tanar) {
            char nume[32] = {0};
            strncpy(nume, a[i].nume, 30);
            printf("%s\n", nume);
        }
    }

    free(a);
}
