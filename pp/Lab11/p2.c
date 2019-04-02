#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE* f = fopen("p2.in", "r+");

    char a, b;
    fscanf(f, "%c %c ", &a, &b);

    long start = ftell(f);

    char s[1000];
    fgets(s, 1000, f);

    int lg = strlen(s);
    for (int i = 0; i < lg; ++i) {
        if (s[i] == a) {
            s[i] = b;
        }
    }

    fseek(f, start, SEEK_SET);

    fprintf(f, "%s\n", s);

    fclose(f);
}
