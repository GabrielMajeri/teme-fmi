#include <stdio.h>

int main() {
    unsigned n;
    scanf("%u", &n);
    
    printf("n*8=%u\n", n << 3);
    printf("n/4=%u\n", n >> 2);
    printf("n*10=%u\n", (n << 1) + (n << 3));
}
