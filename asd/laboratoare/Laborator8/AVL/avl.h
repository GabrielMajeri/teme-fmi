#pragma once

#include <stdbool.h>

typedef struct AVL AVL;

/// Nod/arbore AVL
struct AVL {
    int info;
    int inaltime;
    AVL* st, * dr;
};

/// Alocheaza un nou nod
AVL* avl_nou(int info);
/// Sterge tot arborele
void avl_free(AVL* avl);

/// Afiseaza arborele parcurs SRD
void avl_afisare(AVL* avl);

/// Calculeaza inaltimea subarborelui
int avl_inaltime(AVL* avl);
/// Calculeaza factorul de echilibru al subarborelui
int avl_balance(AVL* avl);

/// Roteste subarborele la dreapta,
/// si returneaza noua radacina.
AVL* avl_rotire_dreapta(AVL* rad);

/// Roteste subarborele la stanga,
/// si returneaza noua radacina.
AVL* avl_rotire_stanga(AVL* rad);

/// Insereaza un nou element, returnand noua radacina.
AVL* avl_inserare(AVL* rad, int elem);

/// Sterge un element din arbore, returnand noua radacina.
AVL* avl_sterge(AVL* rad, int elem);

/// Verifica daca o anumita valoare exista in arbore.
bool avl_cauta(const AVL* rad, int elem);

/// Gaseste maximul din arbore.
int avl_maxim(const AVL* rad);
