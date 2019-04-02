#include <string.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char cnp[14];
    char* nume;
    unsigned varsta, salariu;
} Angajat;

Angajat angajat_nou(const char* cnp, const char* nume, unsigned varsta, unsigned salariu) {
    Angajat ang;

    strncpy(ang.cnp, cnp, 13);

    int n = strlen(nume);
    ang.nume = (char*)calloc(n + 1, sizeof(char));
    strncpy(ang.nume, nume, n);

    ang.varsta = varsta;
    ang.salariu = salariu;

    return ang;
}

void angajat_free(Angajat* ang) {
    free(ang->nume);
    ang->nume = NULL;
}

Angajat angajat_citeste_tastatura(void) {
    char cnp[20];
    fgets(cnp, 13, stdin);

    char nume[100];
    fgets(nume, 99, stdin);

    unsigned varsta;
    scanf("%u", &varsta);

    unsigned salariu;
    scanf("%u", &salariu);

    return angajat_nou(cnp, nume, varsta, salariu);
}

void angajat_scrie(FILE* f, const Angajat* ang) {
    fwrite(ang->cnp, sizeof(char), 13, f);

    unsigned len = strlen(ang->nume);
    fwrite(&len, sizeof(len), 1, f);

    fwrite(ang->nume, sizeof(char), len + 1, f);

    fwrite(&ang->varsta, sizeof(unsigned), 1, f);

    fwrite(&ang->salariu, sizeof(unsigned), 1, f);
}

void angajat_citire(FILE* f, Angajat* ang) {
    fread(ang->cnp, sizeof(char), 13, f);

    unsigned len;
    fread(&len, sizeof(len), 1, f);

    ang->nume = (char*)calloc(len + 1, sizeof(char));

    fread(ang->nume, sizeof(char), len, f);

    fread(&ang->varsta, sizeof(unsigned), 1, f);

    fread(&ang->salariu, sizeof(unsigned), 1, f);
}

void angajat_afisare(const Angajat* ang) {
    printf("Angajat %s:\n", ang->nume);
    printf("Varsta: %d, salariu: %d\n", ang->varsta, ang->salariu);
    printf("\n");
}

int main() {
    FILE* f = fopen("angajati.bin", "wb+");

    int n;
    scanf("%d", &n);

    for (int i = 0; i < n; ++i) {
        Angajat ang = angajat_citeste_tastatura();

        angajat_scrie(f, &ang);

        angajat_free(&ang);
    }

    fclose(f);

    for (int i = 0; i < n; ++i) {
        Angajat ang;
        angajat_citire(f, )
    }

    fclose(f);
}
