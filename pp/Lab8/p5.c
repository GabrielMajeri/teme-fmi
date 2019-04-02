#include <stdlib.h>
#include <stdio.h>

void citeste_vector(FILE* f, int** v, int* n) {
    fscanf(f, "%d", n);

    *v = (int*)malloc(sizeof(int) * *n);

    for (int i = 0; i < *n; ++i) {
        fscanf(f, "%d", *v + i);
    }
}

void push_back(int** v, int* n, int val) {
    int new_n = *n + 1;

    *v = realloc(*v, new_n);

    (*v)[new_n - 1] = val;

    *n = new_n;
}

void afisare(const int* poz, int p, const int* neg, int n) {
    printf("Pozitive = ");
    for (int i = 0; i < p; ++i) {
        printf("%d ", poz[i]);
    }
    printf("\n");

    printf("Negative = ");
    for (int i = 0; i < n; ++i) {
        printf("%d ", neg[i]);
    }
    printf("\n");
}

void sol1(const int* v, int n) {
    int* poz = (int*)malloc(sizeof(int) * n);
    int p = 0;

    int* neg = (int*)malloc(sizeof(int) * n);
    int k = 0;

    for (int i = 0; i < n; ++i) {
        if (v[i] < 0) {
            neg[k++] = v[i];
        } else if (v[i] > 0) {
            poz[p++] = v[i];
        }
    }

    afisare(poz, p, neg, k);

    free(neg);
    free(poz);
}

void sol2(const int* v, int n) {
    int* neg = (int*)malloc(sizeof(int) * 0);
    int k = 0;

    int* poz = (int*)malloc(sizeof(int) * 0);
    int p = 0;

    for (int i = 0; i < n; ++i) {
        if (v[i] < 0) {
            push_back(&neg, &k, v[i]);
        } else if (v[i] > 0) {
            push_back(&poz, &p, v[i]);
        }
    }

    afisare(poz, p, neg, k);

    free(neg);
    free(poz);
}

int main() {
    FILE* f = fopen("p5.in", "r");

    int n;
    int* v;
    citeste_vector(f, &v, &n);

    fclose(f);

    sol1(v, n);
    sol2(v, n);

    free(v);
}
