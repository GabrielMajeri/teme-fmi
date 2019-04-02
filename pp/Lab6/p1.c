#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

// genereaza un intreg random
int genereaza(int n) {
    return (rand() - (RAND_MAX / 2)) % n;
}

// citeste un intreg din fisier
int citeste_int(FILE* f, int i) {
    fseek(f, i * sizeof(int), SEEK_SET);
    int val;
    fread(&val, sizeof(val), 1, f);
    return val;
}

// scrie un intreg in fisier
void scrie_int(FILE* f, int i, int val) {
    fseek(f, i * sizeof(int), SEEK_SET);
    fwrite(&val, sizeof(val), 1, f);
}

// gaseste indicele elementului minim
int gaseste_imin(FILE* f) {
    int min_i = ftell(f) / sizeof(int);
    int min = INT_MAX;

    while (true) {
        int nr;
        fread(&nr, sizeof(nr), 1, f);

        if (feof(f)) {
            return min_i;
        }

        if (nr < min) {
            min = nr;
            min_i = ftell(f) / sizeof(int) - 1;
        }
    }
}

// interschimba numerele de pe pozitiile `i` si `j`
void interschimba(FILE* f, int i, int j) {
    int ival = citeste_int(f, i);
    int jval = citeste_int(f, j);

    scrie_int(f, i, jval);
    scrie_int(f, j, ival);
}

int main() {
    int n;
    scanf("%d", &n);

    FILE* f = fopen("p1.bin", "w+b");

    for (int i = 0; i < n; ++i) {
        int data = genereaza(n);
        fwrite(&data, sizeof(data), 1, f);
    }

    // sortare prin selectie
    for (int i = 0; i < n - 1; ++i) {
        fseek(f, i * sizeof(int), SEEK_SET);

        int imin = gaseste_imin(f);

        interschimba(f, i, imin);
    }

    int max = citeste_int(f, n - 1);
    int count = 1;

    for (int i = 0; i < n - 1; ++i) {
        int nr = citeste_int(f, i);

        count += (nr == max);
    }

    printf("Maximul %d apare de %d ori\n", max, count);

    fclose(f);
}
