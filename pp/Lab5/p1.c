#include <stdlib.h>
#include <stdio.h>

void fr_incr(int* p, int n) {
    p[2000 + n]++;
}

int fr_get(const int* p, int n) {
    return p[2000 + n];
}

int main() {
    FILE* fin = fopen("p1.in", "r");

    int n, m;
    fscanf(fin, "%d %d ", &n, &m);

    int* a = (int*)calloc(sizeof(int), 4001);
    int* b = (int*)calloc(sizeof(int), 4001);

    for (int i = 0; i < n; ++i) {
        int x;
        fscanf(fin, "%d ", &x);

        fr_incr(a, x);
    }

    for (int i = 0; i < m; ++i) {
        int x;
        fscanf(fin, "%d ", &x);

        fr_incr(b, x);
    }

    fclose(fin);

    int comune = 0;
    for (int i = -2000; i <= 2000; ++i) {
        int na = fr_get(a, i), nb = fr_get(b, i);
        if (na && nb) {
            comune += ((na < nb) ? na : nb);
        }
    }

    printf("%d\n", comune);

    free(a);
    free(b);
}
