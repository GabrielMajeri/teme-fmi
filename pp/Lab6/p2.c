#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE* f = fopen("numere_intregi.txt", "r");

    int n;
    fscanf(f, "%d", &n);

    FILE* neg = fopen("p2_neg.bin", "wb");
    FILE* poz = fopen("p2_poz.bin", "wb");

    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(f, "%d", &x);

        if (x < 0) {
            fwrite(&x, sizeof(int), 1, neg);
        } else if (x > 0) {
            fwrite(&x, sizeof(int), 1, poz);
        }
    }

    fclose(neg);
    fclose(poz);
    fclose(f);
}
