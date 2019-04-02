#include <stdio.h>

int main() {
    unsigned int x = 0xAABBCCDDu;

    unsigned int* px = &x;
    unsigned char* octeti = (unsigned char*)px;

    if (octeti[0] == 0xAA) {
        printf("Big endian\n");
    } else {
        printf("Little endian\n");
    }

    for (int i = 0; i < 4; ++i) {
        printf("%X ", octeti[i]);
    }

    printf("\n");
}
