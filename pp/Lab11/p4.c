#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

typedef struct {
    char ch;
    int fr;
} Litera;

int compara(const void* a, const void* b) {
    Litera x = *(const Litera*)a, y = *(const Litera*)b;

    return y.fr - x.fr;
}

int main() {
    FILE* f = fopen("p4.in", "r");

    unsigned frecv[256] = {};

    while (1) {
        char ch = fgetc(f);
        if (feof(f)) {
            break;
        }

        if (isalpha(ch)) {
            ++frecv[(unsigned char)tolower(ch)];
        }
    }

    fclose(f);

    Litera v[256];
    int k = 0;

    for (int i = 0; i < 256; ++i) {
        if (frecv[i]) {
            v[k++] = (Litera){ i, frecv[i] };
        }
    }

    qsort(v, k, sizeof(Litera), compara);

    FILE* g = fopen("p4.out", "w");

    fprintf(g, "%c ", v[0].ch);

    fclose(g);
}
