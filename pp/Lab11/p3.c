#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int main() {
    FILE* f = fopen("p3.in", "r+");

    while (!feof(f)) {
        char ch = fgetc(f);

        if (feof(f)) {
            break;
        }

        if (isalpha(ch)) {
            if (islower(ch)) {
                ch = toupper(ch);
            } else if (isupper(ch)) {
                ch = tolower(ch);
            }
        }

        fseek(f, -1, SEEK_CUR);
        fputc(ch, f);

        fflush(f);
    }

    fclose(f);
}
