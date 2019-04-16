#include "arbore_oarecare.hpp"

#include <queue>

const Nod* ArboreOarecare::getRadacina() const {
    return rad;
}

ArboreOarecare::ArboreOarecare() : rad(nullptr) {
}

ArboreOarecare::ArboreOarecare(const ArboreOarecare& rhs)
    : Arbore(rhs),
      rad(rhs.rad ? new NodFiuFrate(*rhs.rad) : nullptr) {
}

ArboreOarecare::~ArboreOarecare() {
    goleste();
}

void ArboreOarecare::goleste() {
    delete rad;
    rad = nullptr;
}

ArboreOarecare& ArboreOarecare::operator=(const ArboreOarecare& rhs) {
    if (this == &rhs) {
        return *this;
    }

    Arbore::operator=(rhs);
    rad = rhs.rad ? new NodFiuFrate(*rhs.rad) : nullptr;

    return *this;
}

void ArboreOarecare::inserare(const int parinti[], int nrP, int valoare) {
    rad->inserare(parinti, nrP, valoare);
}

std::ostream& operator<<(std::ostream& out, const ArboreOarecare& a) {
    const NodFiuFrate* rad = a.rad;

    if (!rad) {
        out << "(Arbore vid)";
        return out;
    }

    std::queue<const NodFiuFrate*> coada;
    coada.push(rad);

    while (!coada.empty()) {
        const NodFiuFrate* nod = coada.front();
        coada.pop();

        out << "Valoare nod: " << nod->getInfo() << "; ";

        if (nod->getFiu() != nullptr) {
            out << "Valorile fiilor: ";

            for (NodFiuFrate* fiu = nod->getFiu(); fiu != nullptr; fiu = fiu->getFrate()) {
                out << fiu->getInfo() << ' ';
                coada.push(fiu);
            }
        } else {
            out << "Este frunza";
        }

        out << '\n';
    }

    out << "Inaltime: " << a.inaltime() << '\n';
    out << "Nr. noduri: " << a.nrNoduri() << '\n';

    return out;
}

std::istream& operator>>(std::istream& in, ArboreOarecare& a) {
    a.goleste();

    int nrNoduri;
    in >> nrNoduri;

    if (nrNoduri <= 0) {
        throw std::runtime_error("Nu pot citi un arbore vid");
    }

    int info;
    in >> info;
    a.rad = new NodFiuFrate(info);

    for (int i = 1; i < nrNoduri; ++i) {
        int parinti[100], nrP;
        in >> nrP;
        for (int j = 0; j < nrP; ++j) {
            in >> parinti[j];
        }
        int val;
        in >> val;

        a.inserare(parinti, nrP, val);
    }

    return in;
}






















