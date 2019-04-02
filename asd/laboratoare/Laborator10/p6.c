#include <stdio.h>
#include <stdlib.h>

int n, m;
int cioburi[1000][1000];
int minime[1000][1000];

int main() {
    FILE* f = fopen("p6.in", "r");

    fscanf(f, "%d %d", &n, &m);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            fscanf(f, "%d", &cioburi[i][j]);
        }
    }

    fclose(f);
}
