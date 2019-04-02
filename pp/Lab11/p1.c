#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void elimina_litera(char litera, char* cuvant) {
    char* gasit;
    while ((gasit = strchr(cuvant, litera)) != NULL) {
        int i = gasit - cuvant;
        memmove(cuvant + i, cuvant + i + 1, strlen(cuvant) - i + 1);
    }
}

void rezolva(FILE* fin, FILE* fout) {
    char ch;
    fscanf(fin, "%c", &ch);

    char buf[1000];
    char filebuf[10000];
    while (true) {
        fgets(buf, sizeof(buf), fin);

        if (feof(fin)) {
            break;
        }

        elimina_litera(ch, buf);

        sprintf(filebuf, "%s\n", buf);
    }

    fprintf(fout, "%s", filebuf);
}

void fisier_nou() {
    FILE* f = fopen("p1.in", "r");
    FILE* g = fopen("p1.out", "w");

    rezolva(f, g);

    fclose(f);
    fclose(g);
}

void fisier_existent() {
    FILE* f = fopen("p1.inout", "r+");

    rezolva(f, f);

    fclose(f);
}

int main() {
    fisier_existent();
}
