#include <stdio.h>

int main() {
    union {
        short nr;
        struct {
            unsigned char low, high;
        } bytes;
    } u;

    scanf("%hd", &u.nr);

    char tmp = u.bytes.low;
    u.bytes.low = u.bytes.high;
    u.bytes.high = tmp;

    printf("%hd\n", u.nr);
}
