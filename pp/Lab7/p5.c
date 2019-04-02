#include <stdio.h>
#include <stdlib.h>

int cifra(int n, int c) {
    int p = 1, nou = 0;

    while (n) {
        int cif = n % 10;

        if (cif != c) {
            nou = nou + cif * p;
            p *= 10;
        }

        n /= 10;
    }

    return nou;
}

int main() {
    int n = 12312553;
    int c = 3;
    printf("%d fara cifra %d -> %d\n", n, c, cifra(n, c));
}
