#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

bool egale(void* v1, int n1, int dim1, void* v2, int n2, int dim2) {
    if (n1 != n2 || dim1 != dim2) {
        return false;
    }

    return memcmp(v1, v2, n1 * dim1) == 0;
}

int main() {
    int v1[] = { 1, 2, 3 };
    int dim = sizeof(int);
    int n = sizeof(v1) / dim;
    int v2[] = { 1, 2, 3 };

    printf("%d\n", egale(v1, n, dim, v2, n, dim));
}
