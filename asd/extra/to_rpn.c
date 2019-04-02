#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

/// Transforma o expresie din notatie infix in RPN
char* transforma(const char* sursa) {
    // expresia in RPN
    char* rez = (char*)calloc(sizeof(char), 1024);
    int k = 0;

    // stiva de operatori
    char* st_op = (char*)calloc(sizeof(char), 1024);
    int top_op = 0;

    while (*sursa) {
        if (isdigit(*sursa)) {
            // numerele se copiaza in destinatie
            while (isdigit(*sursa)) {
                rez[k++] = *sursa++;
            }
            rez[k++] = ' ';
        } else if (isspace(*sursa)) {
            // consuma spatiile
            while (isspace(*sursa)) {
                ++sursa;
            }
        } else {
            // operator sau paranteza
            char op = *sursa;
            switch (op) {
                case '(':
                    st_op[top_op++] = op;
                    break;

                case ')':
                    while (top_op > 0 && (op = st_op[--top_op]) != '(') {
                        rez[k++] = op;
                        rez[k++] = ' ';
                    }
                    break;

                case '+':
                    st_op[top_op++] = op;
                    break;

                case '-':
                    st_op[top_op++] = op;
                    break;

                case '*':
                    st_op[top_op++] = op;
                    break;

                case '/':
                    st_op[top_op++] = op;
                    break;

                case '~':
                    st_op[top_op++] = op;
                    break;

                default:
                    printf("operator necunoscut: %c\n", op);
                    break;
            }
            ++sursa;
        }
    }

    while (top_op > 0) {
        rez[k++] = st_op[--top_op];
        rez[k++] = ' ';
    }

    free(st_op);

    return rez;
}

int main() {
    const char expr[] = "(3 + 5) * (~2) + 7";

    char* rpn = transforma(expr);

    printf("%s = %s\n", expr, rpn);

    free(rpn);
}
