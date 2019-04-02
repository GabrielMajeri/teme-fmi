#ifndef DUBLA_H
#define DUBLA_H

typedef struct Nod Nod;

struct Nod {
    Nod* st, *dr;
    int info;
};

Nod* nod_nou(int val);

typedef struct Lista Lista;

struct Lista {
    Nod* prim, *ultim;
};

Lista lista_nou(void);
void lista_free(Lista* l);

void lista_adauga_inceput(Lista* l, int info);
void lista_adauga_sfarsit(Lista* l, int info);

void lista_insereaza_dupa(Lista* l, Nod* p, int info);
void lista_insereaza_poz(Lista* l, int k, int info);

void lista_afisare(const Lista* l);
void lista_afisare_invers(const Lista* l);

void lista_sterge_prim(Lista* l);
void lista_sterge_ultim(Lista* l);

void lista_sterge_poz(Lista* l, int k);
void lista_sterge_val(Lista* l, int info);

#endif
