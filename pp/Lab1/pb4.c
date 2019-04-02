#include <stdio.h>

int main() {
    int n;
    scanf("%d", &n);

    printf("%s\n", (n & 1) ? "par" : "impar");
}
