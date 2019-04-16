#pragma once

#include "arbore.hpp"
#include "nod_abc.hpp"

/// Arbore binar de cautare
class ABC : public Arbore {
    NodABC* rad;

    const Nod* getRadacina() const;

public:
    /// Construieste un nou ABC, initial gol
    ABC();

    ABC(const ABC& rhs);

    ~ABC();

    ABC& operator=(const ABC& rhs);

    void goleste() override;

    /// Incearca sa insereze un nod in arbore, returneaza true daca nodul
    /// a fost inserat cu succes.
    bool inserare(int info);

    friend std::ostream& operator<<(std::ostream& out, const ABC& abc);
    friend std::istream& operator>>(std::istream& in, ABC& abc);
};
