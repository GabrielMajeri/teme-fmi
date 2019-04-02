#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// verifica daca un numar natural este prim
bool este_prim(int n) {
    if (n < 2) {
        return false;
    }

    int d = 2;

    // incerc liniar toti divizorii
    while (d <= n / 2) {
        if (n % d == 0) {
            return false;
        }

        ++d;
    }

    return true;
}

// cauta un nr prim apropriat
int nearby_prim(int n) {
    if (este_prim(n)) {
        return n;
    }

    for (int i = 1; ; ++i) {
        if (este_prim(n - i)) {
            return n - i;
        }
        if (este_prim(n + i)) {
            return n + i;
        }
    }
}

int main() {
    int n;
    printf("n = ");
    scanf("%d", &n);

    printf("%d\n", nearby_prim(n));
}
