#include <stdlib.h>
#include <stdio.h>

/// Coada circulara, cu o capacitate fixa, alocata dinamic
typedef struct {
    int* data;
    // lungime si capacitate totala
    int len, cap;
    int start, end;
} Circulara;

/// Alocheaza o noua lista circulara cu o anumita dimensiune
Circulara coada_noua(int capacitate) {
    int* data = (int*)malloc(sizeof(int) * capacitate);

    Circulara c = { data, 0, capacitate, 0, 0 };

    return c;
}

/// Elibereaza memoria pentru lista circulara `c`
void coada_sterge(Circulara* c) {
    free(c->data);
    c->data = NULL;
    c->len = 0;
}

/// Returneaza urmatorul indice
int coada_urmator(const Circulara* c, int i) {
    return (i + 1) % (c->cap + 1);
}

/// Adauga un element la sfarsitul listei
void coada_push(Circulara* c, int valoare) {
    if (c->len == c->cap) {
        printf("Coada este plina\n");
        return;
    }

    c->data[c->end] = valoare;
    c->end = coada_urmator(c, c->end);
    c->len += 1;
}

/// Extrage un element din fata
int coada_pop(Circulara* c) {
    if (c->len == 0) {
        printf("Coada este goala\n");
        return -1;
    }

    int val = c->data[c->start];
    c->start = coada_urmator(c, c->start);
    c->len -= 1;

    return val;
}

/// Afiseaza lista
void coada_afiseaza(const Circulara* c) {
    int i = c->start;
    while (i != c->end) {
        printf("%d ", c->data[i]);
        i = coada_urmator(c, i);
    }
    printf("\n");
}

int main() {
    printf("alocare memorie\n");
    Circulara c = coada_noua(4);

    for (int i = 0; i < 4; ++i) {
        coada_push(&c, i);
        coada_afiseaza(&c);
    }

    // depaseste capacitatea
    coada_push(&c, 4);

    printf("primul = %d\n", coada_pop(&c));

    // acum este spatiu suficient
    coada_push(&c, 4);

    for (int i = 0; i < 4; ++i) {
        printf("%d ", coada_pop(&c));
    }

    printf("\n");

    // coada este goala
    coada_pop(&c);

    printf("eliberare memorie\n");
    coada_sterge(&c);
}
