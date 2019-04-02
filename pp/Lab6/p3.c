#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main() {
    FILE* f = fopen("p1.c", "r");

    char ch;
    while ((ch = getc(f)) != EOF) {
        // posibil comment
        if (ch == '/') {
            char next = getc(f);

            if (next != '/') {
                putchar(ch);
                putchar(next);
                continue;
            } else {
                // citesc pana la sfarsitul liniei
                while ((ch = getc(f)) != '\n')
                    ;
            }
        } else {
            putchar(ch);
        }
    }

    fclose(f);
}
