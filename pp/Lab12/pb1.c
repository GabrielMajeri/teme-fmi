#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

int min(int a, int b) {
    return a < b ? a : b;
}

int lungime_prefix_comun(const char* i, const char* j) {
    int nr = 0;

    while (*i && *j && *i == *j)
        ++i, ++j, ++nr;

    return nr;
}

char* prefix_comun(int n, ...) {
    va_list args;
    va_start(args, n);

    const char* primul = va_arg(args, const char*);

    int lg_max = strlen(primul);

    for (int i = 1; i < n; ++i) {
        const char* sir = va_arg(args, const char*);
        int lg = lungime_prefix_comun(primul, sir);
        lg_max = min(lg_max, lg);
    }

    va_end(args);

    char* comun = (char*)malloc((lg_max + 1) * sizeof(char));

    strncpy(comun, primul, lg_max);
    comun[lg_max] = 0;

    return comun;
}

int main() {
    char* prefix = prefix_comun(4, "ana", "ananana", "ana", "ananas");

    printf("Prefixul comun este '%s'\n", prefix);

    free(prefix);
}
