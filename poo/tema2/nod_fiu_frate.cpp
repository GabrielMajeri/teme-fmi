#include "nod_fiu_frate.hpp"

NodFiuFrate::NodFiuFrate(int valoare)
    : Nod(valoare), fiu(nullptr), frate(nullptr) {
}

NodFiuFrate::NodFiuFrate(const NodFiuFrate& nod)
    : Nod(nod),
      fiu(nod.fiu ? new NodFiuFrate(*nod.fiu) : nullptr),
      frate(nod.frate ? new NodFiuFrate(*nod.frate) : nullptr) {
}

NodFiuFrate::~NodFiuFrate() {
    delete fiu;
    delete frate;
    fiu = frate = nullptr;
}

NodFiuFrate& NodFiuFrate::operator=(const NodFiuFrate& nod) {
    if (this != &nod) {
        Nod::operator=(nod);

        delete fiu;
        delete frate;
        fiu = nod.fiu ? new NodFiuFrate(*nod.fiu) : nullptr;
        frate = nod.frate ? new NodFiuFrate(*nod.frate) : nullptr;
    }
    return *this;
}

NodFiuFrate* NodFiuFrate::getFiu() const {
    return fiu;
}

NodFiuFrate* NodFiuFrate::getFrate() const {
    return frate;
}

NodFiuFrate* NodFiuFrate::adaugaFiu(int valoare) {
    if (!fiu) {
        fiu = new NodFiuFrate(valoare);
        return fiu;
    } else {
        return fiu->adaugaFrate(valoare);
    }
}

NodFiuFrate* NodFiuFrate::adaugaFrate(int valoare) {
    if (!frate) {
        frate = new NodFiuFrate(valoare);
        return frate;
    } else {
        return frate->adaugaFrate(valoare);
    }
}

int NodFiuFrate::inaltime() const {
    int h = fiu ? fiu->inaltime() : 0;
    if (frate != nullptr) {
        h = std::max(h, frate->inaltime());
    }
    return 1 + h;
}

int NodFiuFrate::nrNoduri() const {
    return 1 + (fiu ? fiu->nrNoduri() : 0) + (frate ? frate->nrNoduri() : 0);
}

void NodFiuFrate::inserare(const int parinti[], int nrP, int valoare) {
    if (nrP == 0) {
        adaugaFiu(valoare);
    } else {
        NodFiuFrate* ptr = fiu;

        for (int i = 0; i < parinti[0]; ++i) {
            ptr = ptr->getFrate();
            if (ptr == nullptr) {
                throw std::runtime_error("Nu pot insera un fiu la un nod inexistent");
            }
        }

        ptr->inserare(parinti + 1, nrP - 1, valoare);
    }
}


