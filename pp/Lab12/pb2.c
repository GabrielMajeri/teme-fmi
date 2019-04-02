#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

int nr_aparitii(int x, int n, ...) {
    va_list ap;
    va_start(ap, n);

    int count = 0;

    for (int i = 0; i < n; ++i) {
        int elem = va_arg(ap, int);

        if (elem == x) {
            ++count;
        }
    }

    va_end(ap);

    return count;
}

int main() {
    int v[4];
    scanf("%d %d %d %d", &v[0], &v[1], &v[2], &v[3]);

    bool distincte = true;

    for (int i = 0; i < 4; ++i) {
        distincte &= nr_aparitii(v[i], 4, v[0], v[1], v[2], v[3]) == 1;
    }

    printf("Distincte: %d\n", (int)distincte);
}
