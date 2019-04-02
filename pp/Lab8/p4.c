#include <stdio.h>
#include <stdlib.h>

typedef struct {
    float re, im;
} Complex;

void print_complex(const Complex* z) {
    printf("%f + %f i\n", z->re, z->im);
}

float modul(const Complex* z) {
    return z->re * z->re + z->im * z->im;
}

void max_modul(const Complex* v, int n) {
    const Complex* max = v;

    for (int i = 1; i < n; ++i) {
        if (modul(max) < modul(v + i)) {
            max = v + i;
        }
    }

    print_complex(max);
}

int main() {
    FILE* f = fopen("p4.in", "r");

    int n;
    fscanf(f, "%d", &n);

    Complex* v = (Complex*)calloc(sizeof(Complex), n);

    for (int i = 0; i < n; ++i) {
        Complex* z = v + i;
        fscanf(f, "%f %f", &z->re, &z->im);
    }

    fclose(f);

    max_modul(v, n);

    free(v);
}
