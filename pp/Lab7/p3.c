#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

bool este_munte(int n) {
    bool crescator = true;
    bool schimbat = false;
    int ultim_cif = n % 10;
    n /= 10;

    while (n) {
        int cif = n % 10;

        if (crescator) {
            if (cif <= ultim_cif) {
                if (schimbat) {
                    return false;
                }

                schimbat = true;
                crescator = false;
            }
        } else {
            if (cif >= ultim_cif) {
                return false;
            }
        }

        ultim_cif = cif;
        n /= 10;
    }

    return schimbat == true;
}

int main() {
    int a;
    printf("a = ");
    scanf("%d", &a);

    int b;
    printf("b = ");
    scanf("%d", &b);

    for (int x = a; x <= b; ++x) {
        if (este_munte(x)) {
            printf("%d\n", x);
        }
    }
}
