#include <stdio.h>

int max2(int a, int b) {
    return a > b ? a : b;
}

int max3(int a, int b, int c) {
    int m1 = max2(a, b);
    int m2 = max2(b, c);
    return max2(m1, m2);
}

int main() {
    int a, b, c;
    scanf("%d %d %d", &a, &b, &c);
    
    printf("%d\n", max3(a, b, c));
}
