#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void replace(const void* x, const void* y, void* v, int n, int dim) {
    for (int i = 0; i < n; ++i) {
        const char* a = (const char*)x;
        char* b = (char*)v + i * dim;

        if (memcmp(a, b, dim) == 0) {
            memcpy(b, y, dim);
        }
    }
}

void print_vec(const float v[], int n) {
    for (int i = 0; i < n; ++i) {
        printf("%.2f ", v[i]);
    }
    printf("\n");
}

int main() {
    float v[] = { 2, 3, 4.5, 1, -2.1, 4.5 };
    int n = sizeof(v) / sizeof(v[0]);

    print_vec(v, n);

    const float x = 4.5;
    const float y = 0;
    replace(&x, &y, v, n, sizeof(int));

    print_vec(v, n);
}
