#pragma once

#include "nod.hpp"

/// Nod dintr-un arbore oarecare in reprezentare inlantuita
class NodFiuFrate : public Nod {
    NodFiuFrate* fiu;
    NodFiuFrate* frate;

public:
    /// Construieste o noua frunza cu o anumita valoare.
    NodFiuFrate(int valoare);

    NodFiuFrate(const NodFiuFrate& nod);

    ~NodFiuFrate() override;

    NodFiuFrate& operator=(const NodFiuFrate& nod);

    int inaltime() const override;
    int nrNoduri() const override;

    /// Returneaza fiul acestui nod.
    NodFiuFrate* getFiu() const;
    /// Returneaza fratele acestui nod.
    NodFiuFrate* getFrate() const;

    /// Adauga un nou descendent direct la acest nod.
    NodFiuFrate* adaugaFiu(int valoare);
    /// Adauga un nou nod la acelasi nivel cu acesta.
    NodFiuFrate* adaugaFrate(int valoare);

    /// Insereaza un nod care are o anumita lista de parinti.
    void inserare(const int parinti[], int nrP, int valoare);
};
