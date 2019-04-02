#include <stdlib.h>
#include <stdio.h>

int main() {
    FILE* f = fopen("bitonica.in", "r");

    int n;
    fscanf(f, "%d", &n);

    int* v = (int*)malloc(sizeof(int) * n);

    for (int i = 0; i < n; ++i) {
        fscanf(f, "%d", v + i);
    }

    int* rez = (int*)malloc(sizeof(int) * n);
    int k = 0;

    int* st = v, *dr = v + n - 1;

    if (st[0] < st[1]) {
        while (st < dr) {
            if (*st <= *dr) {
                rez[k++] = *st++;
            } else {
                rez[k++] = *dr--;
            }
        }
    } else {
        k = n - 1;
        while (st < dr) {
            if (*st >= *dr) {
                rez[k--] = *st++;
            } else {
                rez[k--] = *dr--;
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        printf("%d ", rez[i]);
    }

    free(rez);
    free(v);
    fclose(f);

    printf("\n");
}
