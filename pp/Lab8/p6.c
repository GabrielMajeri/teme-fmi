#include <stdio.h>
#include <stdlib.h>

int find_max(const int* v, int n) {
    int max = v[0];
    for (int i = 1; i < n; ++i) {
        if (max < v[i]) {
            max = v[i];
        }
    }
    return max;
}

int main() {
    FILE* f = fopen("p6.in", "r");

    int n;
    fscanf(f, "%d", &n);

    int* v = (int*)malloc(sizeof(int) * n);

    for (int i = 0; i < n; ++i) {
        fscanf(f, "%d", v + i);
    }

    fclose(f);

    int max = find_max(v, n);

    int* pozitii = malloc(sizeof(int) * 0);
    int k = 0;

    for (int i = 0; i < n; ++i) {
        if (v[i] == max) {
            pozitii = realloc(pozitii, k + 1);
            pozitii[k] = i;
            ++k;
        }
    }

    free(v);

    printf("Pozitiile maximului sunt: \n");
    for (int i = 0; i < k; ++i) {
        printf("%d ", pozitii[i]);
    }

    printf("\n");
    free(pozitii);
}
