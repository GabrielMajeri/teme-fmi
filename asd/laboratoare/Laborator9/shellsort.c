#include <stdlib.h>
#include <stdio.h>

void afisare(const int* v, int n) {
    for (int i = 0; i < n; ++i) {
        printf("%d ", v[i]);
    }
    printf("\n");
}

void insertion_sort(int v[], int n, int increment) {
    for (int i = increment; i < n; i += increment) {
        const int aux = v[i];
        int j;
        for (j = i; j >= increment && aux < v[j - increment]; j -= increment) {
            v[j] = v[j - increment];
        }
        v[j] = aux;
    }
}

void shell_sort(int v[], int n) {
    const int incrementi[] = { 12, 9, 8, 6, 4, 3, 2, 1 };
    const int nr_incrementi = sizeof(incrementi) / sizeof(int);

    for (int i = 0; i < nr_incrementi; ++i) {
        int increment = incrementi[i];
        if (increment >= n) {
            continue;
        }

        insertion_sort(v, n, increment);
    }
}

int main() {
    FILE* f = fopen("sort.in", "r");

    int n;
    fscanf(f, "%d", &n);

    int* v = (int*)malloc(n * sizeof(int));

    for (int i = 0; i < n; ++i) {
        fscanf(f, "%d", v + i);
    }

    fclose(f);

    shell_sort(v, n);
    afisare(v, n);

    free(v);
}
