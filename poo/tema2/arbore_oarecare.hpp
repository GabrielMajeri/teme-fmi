#pragma once

#include "arbore.hpp"
#include "nod_fiu_frate.hpp"

/// Arbore oarecare in reprezentare inlantuita
class ArboreOarecare : public Arbore {
    NodFiuFrate* rad;

    const Nod* getRadacina() const;

public:
    /// Construieste un nou arbore vid
    ArboreOarecare();

    ArboreOarecare(const ArboreOarecare& rhs);

    ~ArboreOarecare();

    ArboreOarecare& operator=(const ArboreOarecare& rhs);

    void goleste() override;

    /// Insereaza un nod in arbore, locul acestuia fiind determinat te lista de parinti.
    void inserare(const int parinti[], int nrP, int valoare);

    friend std::ostream& operator<<(std::ostream& out, const ArboreOarecare& a);
    friend std::istream& operator>>(std::istream& in, ArboreOarecare& a);
};
