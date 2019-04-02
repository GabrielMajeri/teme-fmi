#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

/// Citeste caractere pana la spatiu/NULL, pentru a forma un numar
int citeste_numar(const char** p) {
    int nr = 0;
    while (**p && isdigit(**p)) {
        nr = (nr * 10) + (**p - '0');
        ++*p;
    }
    return nr;
}

/// Citeste caractere spatiu pana se termina
void citeste_spatiu(const char** p) {
    while (isspace(**p)) {
        ++*p;
    }
}

/// Evalueaza o expresie RPN cu o stiva
int eval_stiva(const char* expr) {
    const char* p = expr;

    int* stiva = (int*)calloc(1000, sizeof(int));
    int len = 0;

    while (*p) {
        if (isdigit(*p)) {
            stiva[len++] = citeste_numar(&p);
            continue;
        }

        if (isspace(*p)) {
            citeste_spatiu(&p);
            continue;
        }

        int a = stiva[len - 2], b = stiva[len - 1];
        int rezultat;

        switch (*p) {
            case '+':
                rezultat = a + b;
                break;
            case '-':
                rezultat = a - b;
                break;
            case '*':
                rezultat = a * b;
                break;
            case '/':
                rezultat = a / b;
                break;
            default:
                printf("Operator necunoscut: %c", *p);
                rezultat = 0;
                break;
        }

        len -= 2;

        stiva[len++] = rezultat;
        ++p;
    }

    if (len != 1) {
        printf("Stiva contine numere care nu au intrat in expresie\n");
    }

    int rezultat = stiva[0];

    free(stiva);

    return rezultat;
}

typedef enum {
    CONSTANTA,
    PLUS,
    MINUS,
    MUL,
    DIV,
} TipExpresie;

typedef struct Expresie Expresie;

/// Un arbore de expresie
struct Expresie {
    TipExpresie tip;
    int valoare;
    Expresie* st, * dr;
};

Expresie* arbore_nou(const char* expr) {
    const char* p = expr;

    Expresie** stiva = (Expresie**)calloc(1000, sizeof(Expresie*));
    int len = 0;

    while (*p) {
        if (isdigit(*p)) {
            Expresie* e = (Expresie*)malloc(sizeof(Expresie));
            e->tip = CONSTANTA;
            e->valoare = citeste_numar(&p);
            e->st = e->dr = NULL;

            stiva[len++] = e;

            continue;
        }

        if (isspace(*p)) {
            citeste_spatiu(&p);

            continue;
        }

        Expresie* a = stiva[len - 2], * b = stiva[len - 1];
        Expresie* e = (Expresie*)malloc(sizeof(Expresie));
        e->st = a;
        e->dr = b;

        switch (*p) {
            case '+':
                e->tip = PLUS;
                break;
            case '-':
                e->tip = MINUS;
                break;
            case '*':
                e->tip = MUL;
                break;
            case '/':
                e->tip = DIV;
                break;
            default:
                printf("Operator necunoscut: %c", *p);
                break;
        }

        len -= 2;

        stiva[len++] = e;
        ++p;
    }

    if (len != 1) {
        printf("Stiva contine numere care nu au intrat in expresie\n");
    }

    Expresie* rezultat = stiva[0];

    free(stiva);

    return rezultat;
}

int eval_arbore(const Expresie* e) {
    if (e == NULL) {
        printf("Nu se poate evalua o expresie nula\n");
        return -1;
    }

    Expresie* st = e->st, *dr = e->dr;

    switch (e->tip) {
        case CONSTANTA:
            return e->valoare;
        case PLUS:
            return eval_arbore(st) + eval_arbore(dr);
        case MINUS:
            return eval_arbore(st) - eval_arbore(dr);
        case MUL:
            return eval_arbore(st) * eval_arbore(dr);
        case DIV:
            return eval_arbore(st) / eval_arbore(dr);
        default:
            printf("Tip necunoscut de expresie\n");
            return -1;
    }
}

void sterge_arbore(Expresie* e) {
    if (e == NULL) {
        return;
    }

    sterge_arbore(e->st);
    sterge_arbore(e->dr);

    free(e);
}

int main() {
    const char expr[] = "1 2 + 4 * 5 + 3 -";

    printf("%d\n", eval_stiva(expr));

    Expresie* arbore = arbore_nou(expr);

    printf("%d\n", eval_arbore(arbore));

    sterge_arbore(arbore);
}
