#include <stdio.h>
#include <stdlib.h>

#include "cod.h"

void problema1() {
    printf("Problema 1:\n");

    char ch = getchar();

    ch = upper_to_lower_case(ch);

    printf("%c\n", ch);
}

void problema2() {
    printf("Problema 2:\n");

    int a = parse_int();
    char op = parse_operator();
    int b = parse_int();

    int rezultat = eval_expr(a, op, b);
    printf("%d\n", rezultat);
}

void problema3() {
    printf("Problema 3:\n");

    int nr;
    scanf("%d\n", &nr);

    candidat* v = (candidat*)calloc(nr, sizeof(candidat));

    int n = 0;
    for (int i = 0; i < nr; ++i) {
        candidat c;
        citeste_candidat(&c);
        insereaza_candidat(v, &n, &c);
    }

    meniu_elevi(c, n);
}

int main() {
    //problema1();
    //problema2();
    problema3();
}
