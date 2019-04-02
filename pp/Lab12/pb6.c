#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

void invers(int n, int* rez) {
    if (n != 0) {
        int cifra = n % 10;

        *rez = *rez * 10 + cifra;

        invers(n / 10, rez);
    }
}

bool este_palindrom(int n) {
    int x = 0;
    invers(n, &x);
    return n == x;
}

int main() {
    int x;
    scanf("%d", &x);

    printf("este %d palindrom: %s\n", x, este_palindrom(x) ? "adev" : "fals");
}
