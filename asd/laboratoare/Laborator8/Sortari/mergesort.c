#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void afisare(const int* v, int n) {
    for (int i = 0; i < n; ++i) {
        printf("%d ", v[i]);
    }
}

void interclasare(int* st, int* const mid, int* dr) {
    int n = dr - st;
    int* aux = (int*)malloc(sizeof(int) * n);

    int* i = st, * j = mid;
    int k = 0;

    while (i < mid && j < dr) {
        if (*i <= *j) {
            aux[k++] = *i++;
        } else {
            aux[k++] = *j++;
        }
    }

    while (i < mid) {
        aux[k++] = *i++;
    }

    while (j < dr) {
        aux[k++] = *j++;
    }

    --k;

    for (; k >= 0; --k) {
        st[k] = aux[k];
    }

    free(aux);
}

void swap(int* a, int* b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

void insertion_sort(int* v, int n) {
    for (int i = 1; i <= n; ++i) {
        int de_inserat = v[i];
        int j = 0;

        while (j < i) {
            if (v[j] > v[i]) {
                break;
            }

            ++j;
        }

        for (int k = i; k > j; --k) {
            v[k] = v[k - 1];
        }

        v[j] = de_inserat;
    }
}

void mergesort(int* v, int st, int dr, bool cu_insertie) {
    if (dr - st < 2) {
        if (v[st] > v[dr]) {
            swap(v + st, v + dr);
        }
        return;
    }

    const int lungime = dr - st;

    if (cu_insertie && lungime <= 10) {
        insertion_sort(v + st, lungime);
        return;
    }

    int mid = st + (dr - st) / 2;

    mergesort(v, st, mid, cu_insertie);
    mergesort(v, mid, dr, cu_insertie);

    interclasare(v + st, v + mid, v + dr);
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

    bool cu_insertie = false;
    mergesort(v, 0, n - 1, cu_insertie);

    afisare(v, n);
    printf("\n");

    fclose(f);
}
