#include <stdlib.h>
#include <stdio.h>

int main() {
    FILE* f = fopen("p7.in", "r");

    int n;
    fscanf(f, "%d", &n);

    float* v = (float*)malloc(sizeof(float) * n);

    for (int i = 0; i < n; ++i) {
        fscanf(f, "%f", v + i);
    }

    fclose(f);

    float* h = (float*)malloc(0);


    int lg = n + (n - 1);
    for (int i = 0; i < lg; ++i) {
        printf("%f ", h[i]);
    }

    printf("\n");

    free(h);

    free(v);
}
