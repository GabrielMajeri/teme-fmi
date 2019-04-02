#include <stdio.h>
#include <stdbool.h>
#include "dubla/dubla.h"

void lista_teste(void) {
    Lista l = lista_nou();

    lista_adauga_inceput(&l, 5);
    lista_adauga_inceput(&l, -3);
    lista_adauga_inceput(&l, 7);
    lista_adauga_sfarsit(&l, 6);
    lista_adauga_sfarsit(&l, 2);
    lista_insereaza_poz(&l, 3, 1);

    lista_afisare(&l);

    lista_sterge_val(&l, 7);
    lista_sterge_poz(&l, 2);

    lista_afisare(&l);

    lista_free(&l);
}

int main() {
    Lista l = lista_nou();

    bool ruleaza = true;
    while (ruleaza) {
        char cmd = ' ';
        int n = scanf("%c", &cmd);

        if (n == EOF) {
            break;
        }

        if (cmd == '\n') {
            continue;
        }

        switch (cmd) {
            case 'a':
                {
                    printf("Inserare element in fata: \n");

                    int n;
                    scanf("%d", &n);

                    lista_adauga_inceput(&l, n);
                }
                break;

            case 'b':
                {
                    printf("Inserare element la final: \n");

                    int n;
                    scanf("%d", &n);

                    lista_adauga_sfarsit(&l, n);
                }
                break;

            case 'c':
                {
                    printf("Element si pozitie: \n");

                    int n, k;
                    scanf("%d %d", &n, &k);

                    lista_insereaza_poz(&l, k, n);
                }
                break;

            case 'd':
                {
                    printf("Afisare in ordine: \n");

                    lista_afisare(&l);
                }
                break;

            case 'e':
                {
                    printf("Afisare in ordine inversa: \n");

                    lista_afisare_invers(&l);
                }
                break;

            case 'f':
                {
                    printf("Pozitie de sters: \n");

                    int k;
                    scanf("%d", &k);
                }
                break;

            case 'g':
                {
                    printf("Valoare de sters: \n");

                    int x;
                    scanf("%d", &x);

                    lista_sterge_val(&l, x);
                }
                break;

            default:
                printf("Comanda necunoscuta: %d\n", cmd);
                ruleaza = false;
                break;
        }
    }

    lista_free(&l);
}
