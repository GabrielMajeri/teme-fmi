#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

void swap(int* x, int* y) {
    int aux = *x;
    *x = *y;
    *y = aux;
}

int partitionare(int* v, int st, int dr) {
    int ipiv = st + rand() % (dr - st + 1);
    int pivot = v[ipiv];

    while (1) {
        while (v[st] < pivot) {
            ++st;
        }

        while (v[dr] > pivot) {
            --dr;
        }

        if (st >= dr) {
            return dr;
        }

        swap(v + st, v + dr);

        ++st;
        --dr;
    }
}

void insertion_sort(int v[], int n) {
    for (int i = 1; i < n; ++i) {
        const int aux = v[i];
        int j;
        for (j = i; 1 <=j && aux < v[j - 1]; --j) {
            v[j] = v[j - 1];
        }
        v[j] = aux;
    }
}

void quicksort(int* v, int st, int dr, bool use_insertion) {
    if (dr <= st) {
        return;
    }

    if (use_insertion && dr - st <= 10) {
        insertion_sort(v + st, dr - st + 1);
        return;
    }

    int pivot = partitionare(v, st, dr);

    quicksort(v, st, pivot, use_insertion);
    quicksort(v, pivot + 1, dr, use_insertion);
}

void afisare(const int* v, int n) {
    for (int i = 0; i < n; ++i) {
        printf("%d ", v[i]);
    }
}

int main() {
    FILE* f = fopen("sort.in", "r");

    int n;
    fscanf(f, "%d", &n);

    int* v = (int*)malloc(sizeof(int) * n);

    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(f, "%d", &x);
        v[i] = x;
    }

    fclose(f);

    srand(time(NULL));

    quicksort(v, 0, n - 1, true);

    afisare(v, n);
    printf("\n");

    free(v);
}
