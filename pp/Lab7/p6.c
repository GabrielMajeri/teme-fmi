#include <stdio.h>
#include <stdlib.h>

void permuta_circular(int* v, int n, int m) {
    for (int k = 0; k < m; ++k) {
        int ultimul = v[n - 1];

        for (int i = n - 1; i >= 1; --i) {
            v[i] = v[i - 1];
        }

        v[0] = ultimul;
    }
}

int main() {
    int linii, coloane;
    int m[5][5];

    scanf("%d %d", &linii, &coloane);

    for (int i = 0; i < linii; ++i) {
        for (int j = 0; j < coloane; ++j) {
            scanf("%d", &m[linii][coloane]);
       }
    }

    for (int i = 0; i < linii; ++i) {
        for (int j = 0; j < coloane; ++j) {
            permuta_circular(m[i], coloane, i);
        }
    }

    printf("Matricea este:\n");

    for (int i = 0; i < linii; ++i) {
        for (int j = 0; j < coloane; ++j) {
            printf("%d ", m[i][j]);
        }
        printf("\n");
    }
}
