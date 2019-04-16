#pragma once

#include <iostream>

/// Clasa abstracta reprezentand un nod dintr-un arbore.
class Nod {
    /// Informatia retinuta in acest nod
    int info;

protected:
    /// Construieste un nou nod initializat cu o anumita valoare
    Nod(int valoare);

public:
    virtual ~Nod();

    /// Getter pentru informatia nodului
    int getInfo() const;

    /// Calculeaza inaltimea subarborelui acestui nod
    virtual int inaltime() const = 0;
    /// Calculeaza numarul de noduri aflati in subarborele acestui nod
    virtual int nrNoduri() const = 0;
};
