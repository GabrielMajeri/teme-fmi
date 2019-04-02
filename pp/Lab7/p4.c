#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long u64;

u64 inverseaza(u64 nr) {
    u64 inv = 0;

    while (nr) {
        inv = inv * 10 + nr % 10;
        nr /= 10;
    }

    return inv;
}

void descompune(u64 n, u64* const pare, u64* const impare) {
    *pare = 0;
    *impare = 0;

    n = inverseaza(n);

    while (n) {
        *pare = *pare * 10 + n % 10;

        n /= 10;

        if (!n) {
            break;
        }

        *impare = *impare * 10 + n % 10;

        n /= 10;
    }
}

int main() {
    u64 n;
    scanf("%lld", &n);

    u64 pare, impare;
    descompune(n, &pare, &impare);

    printf("%lld %lld\n", pare, impare);
}
