#ifndef COD_H
#define COD_H 1

#include <stdbool.h>

char upper_to_lower_case(char ch);

int parse_int();
char parse_operator();

int eval_expr(int a, char op, int b);

typedef struct {
    int nr_legitimatie;
    char nume[64];
    double nota_mate, nota_info, nota_bac;
    double medie;
    bool admis, buget;
} candidat;

#define AVERAGE(a, b) \
    (0.5 * ((a) + (b)))

#define NOTA_ADMITERE(c) \
    (AVERAGE((c)->nota_mate, (c)->nota_info))

#define MEDIE_ADMITERE(c) \
    (0.8 * NOTA_ADMITERE(c) + 0.2 * (c)->nota_bac)

#define MEDIE_MINIMA 5.0

void citeste_candidat(candidat* c);

void insereaza_candidat(candidat v[], int* n, const candidat* c);

void completeaza_buget(candidat v[], int n);

void afisare_elevi(const candidat* v, int n);

void meniu_elevi(const candidat* v, int n);

#endif // COD_H
