#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

// calculeaza suma divizorilor lui x,
// in afara de x insusi
int sumad(int x) {
    int s = 0;

    for (int i = 1; i <= x / 2; ++i) {
        if (x % i == 0) {
            s += i;
        }
    }

    return s;
}

int main() {
    int n;
    printf("n = ");
    scanf("%d", &n);

    for (int a = 1; a < n; ++a) {
        int b = sumad(a);

        if (a < b && b < n && sumad(b) == a) {
            printf("(%d, %d)\n", a, b);
        }
    }

    printf("\n");
}
