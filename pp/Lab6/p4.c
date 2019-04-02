#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int id;
    char nume[21];
    char prenume[21];
    float nota;
} student;

void creare(const char* path, int n) {
    FILE* f = fopen(path, "wb");

    for (int i = 0; i < n; ++i) {
        student s;
        scanf("%d %20s %20s %f", &s.id, s.nume, s.prenume, &s.nota);

        fwrite(&s, sizeof(student), 1, f);
    }

    fclose(f);
}

void afisare(const char* src, const char* dest) {
    FILE* f = fopen(src, "rb");

    if (f == NULL) {
        fprintf(stderr, "Nu se poate deschide fisierul %s\n", src);
        return;
    }

    fseek(f, 0, SEEK_END);
    long end = ftell(f);
    int n = end / sizeof(student);

    rewind(f);

    FILE* g = fopen(dest, "w");

    if (g == NULL) {
        fprintf(stderr, "Nu se poate deschide fisierul %s\n", dest);
        return;
    }

    for (int i = 0; i < n; ++i) {
        student s;
        fread(&s, sizeof(student), 1, f);

        fprintf(g, "%d %s %s %f\n", s.id, s.nume, s.prenume, s.nota);
    }

    fclose(g);
    fclose(f);
}

void modificare(const char* path, int id, float nota) {
    FILE* f = fopen(path, "rb+");

    if (f == NULL) {
        fprintf(stderr, "Nu se poate deschide fisierul %s\n", path);
        return;
    }

    fseek(f, 0, SEEK_END);
    long end = ftell(f);
    int n = end / sizeof(student);

    for (int i = 0; i < n; ++i) {
        student s;
        fread(&s, sizeof(student), 1, f);

        if (s.id == id) {
            s.nota = nota;

            fseek(f, -sizeof(student), SEEK_CUR);

            fwrite(&s, sizeof(student), 1, f);
        }
    }

    fclose(f);
}

void adaugare(const char* path, student s) {
    FILE* f = fopen(path, "ab");

    fseek(f, 0, SEEK_END);

    fwrite(&s, sizeof(student), 1, f);

    fclose(f);
}

int main() {
    int n;
    scanf("%d", &n);

    creare("studenti.bin", n);

    modificare("studenti.bin", 22, 5.4);

    student s;

    s.id = 13;
    strcpy(s.nume, "Z");
    strcpy(s.prenume, "Vasile");
    s.nota = 6.7;

    adaugare("studenti.bin", s);

    afisare("studenti.bin", "studenti.txt");
}
