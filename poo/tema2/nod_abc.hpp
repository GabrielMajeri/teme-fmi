#pragma once

#include "nod.hpp"

/// Nod dintr-un arbore binar de cautare
class NodABC : public Nod {
    NodABC* st;
    NodABC* dr;

public:
    NodABC(int valoare);
    NodABC(int valoare, NodABC* stanga, NodABC* dreapta);

    NodABC(const NodABC& nod);
    ~NodABC() override;

    NodABC& operator=(const NodABC& abc);

    int inaltime() const override;
    int nrNoduri() const override;

    /// Afiseaza in preordine.
    void afisareRSD(std::ostream& out) const;
    /// Afiseaza in inordine.
    void afisareSRD(std::ostream& out) const;
    /// Afiseaza in postordine.
    void afisareSDR(std::ostream& out) const;

    /// Insereaza o valoare in ABC.
    /// Returneaza true daca s-a inserat cu succes.
    bool inserare(int info);
};
