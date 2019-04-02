#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main() {
    FILE* f = fopen("p3.in", "r");

    char sa[101], sb[101];
    fscanf(f, "%100s", sa);
    fscanf(f, "%100s", sb);

    fclose(f);

    int n = strlen(sa), m = strlen(sb);

    int* p = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i) {
        p[i] = sa[n - i - 1] - '0';
    }

    int* q = (int*)malloc(m * sizeof(int));
    for (int i = 0; i < m; ++i) {
        q[i] = sb[m - i - 1] - '0';
    }

    int max = (n < m) ? m : n;
    int min = (n < m) ? n : m;

    int* rez = (int*)malloc((max + 1) * sizeof(int));

    int k = 0;
    int carry = 0;

    while (k < min) {
        int suma = p[k] + q[k];

        if (carry) {
            suma += carry;
            carry = 0;
        }

        rez[k] = suma % 10;

        carry = suma / 10;

        ++k;
    }

    while (k < n) {
        int suma = p[k] + carry;

        if (carry) {
            suma += carry;
            carry = 0;
        }

        rez[k] = suma % 10;
        carry = suma / 10;

        ++k;
    }

    while (k < m) {
        int suma = q[k] + carry;

        if (carry) {
            suma += carry;
            carry = 0;
        }

        rez[k] = suma % 10;
        carry = suma / 10;

        ++k;
    }

    free(q);
    free(p);

    for (int i = 0; i < k; ++i) {
        printf("%c", rez[k - i - 1] + '0');
    }

    printf("\n");

    free(rez);
}
