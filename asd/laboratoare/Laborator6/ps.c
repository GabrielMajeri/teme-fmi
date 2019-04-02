#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int lg_pref_max(const char* sir, int n) {
    // pref_max[i] = lungimea prefixului maxim din sir[0..i]

    int* pref_max = (int*)malloc(sizeof(int) * n);

    // sunt 0 prefixe de lungime 0
    pref_max[0] = 0;

    int i = 1, lg = 0;

    // incerc prefixele diferite de sir insusi
    while (i < n) {
        // daca pot prelungi prefixul
        if (sir[i] == sir[lg]) {
            ++lg;
            pref_max[i++] = lg;
        } else { // nu pot prelungi prefixul
            if (lg == 0) {
                pref_max[i++] = 0;
            } else {
                // prefixul maxim este maximul precedent
                lg = pref_max[lg - 1];
            }
        }
    }

    int max = pref_max[n - 1];

    free(pref_max);

    return max;
}

int main() {
    // citire
    char* sir = (char*)calloc(sizeof(char), 1024);
    scanf("%1000s", sir);

    int max = lg_pref_max(sir, strlen(sir));

    // afisare
    for (int i = 0; i < max; ++i) {
        printf("%c", sir[i]);
    }

    printf("\n");

    free(sir);
}
