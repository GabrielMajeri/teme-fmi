#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "deque/deque.h"

int main() {
    Deque d = deque_nou();

    bool ruleaza = true;
    while (ruleaza) {
        char cmd = ' ';
        fscanf(stdin, "%c", &cmd);

        if (feof(stdin)) {
            break;
        }

        if (cmd == '\n') {
            continue;
        }

        switch (cmd) {
            case 'a':
                {
                    printf("Push front: \n");

                    int x;
                    scanf("%d", &x);

                    deque_push_front(&d, x);
                }
                break;

            case 'b':
                {
                    int x = deque_pop_front(&d);

                    printf("Pop front: %d\n", x);
                }
                break;

            case 'c':
                {
                    printf("Push back: \n");

                    int x;
                    scanf("%d", &x);

                    deque_push_back(&d, x);
                }
                break;

            case 'd':
                {
                    int x = deque_pop_back(&d);

                    printf("Pop back: %d\n", x);
                }
                break;

            default:
                printf("Comanda necunoscuta: %d\n", cmd);
                ruleaza = false;
                break;
        }
    }

    deque_free(&d);
}
