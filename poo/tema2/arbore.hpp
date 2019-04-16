#pragma once

#include "nod.hpp"

/// Clasa de baza abstracta pentru arbori cu radacina
class Arbore {
protected:
    virtual const Nod* getRadacina() const = 0;

public:
    virtual ~Arbore();

    /// Sterge toate nodurile din arbore
    virtual void goleste() = 0;

    /// Calculeaza inaltimea arborelui
    int inaltime() const;
    /// Calculeaza nr. de noduri din arbore
    int nrNoduri() const;
};
