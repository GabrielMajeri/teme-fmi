#include <stdlib.h>
#include <stdio.h>

int caut_bin(int cautat, const int* v, int st, int dr) {
    while (st < dr) {
        int mid = st + (dr - st) / 2;

        if (cautat < v[mid]) {
            dr = mid - 1;
        } else if (v[mid] < cautat) {
            st = mid + 1;
        } else {
            return mid;
        }
    }

    return -1;
}

int cauta(int cautat, const int* v, int n) {
    return caut_bin(cautat, v, 0, n - 1);
}

int main() {
    FILE* fin = fopen("p2.in", "r");

    int n;
    fscanf(fin, "%d ", &n);

    int* v = (int*)malloc(sizeof(int) * n);

    for (int i = 0; i < n; ++i) {
        fscanf(fin, "%d ", v + i);
    }

    int x, y;
    fscanf(fin, "%d %d ", &x, &y);

    fclose(fin);

    int a = cauta(x, v, n), b = cauta(y, v, n);

    for (int i = a; i <= b; ++i) {
        printf("%d ", v[i]);
    }

    printf("\n");

    free(v);
}
