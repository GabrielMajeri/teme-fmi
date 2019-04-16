#include "nod_abc.hpp"

NodABC::NodABC(int valoare): NodABC(valoare, nullptr, nullptr) {
}

NodABC::NodABC(int valoare, NodABC* stanga, NodABC* dreapta)
    : Nod(valoare), st(stanga), dr(dreapta) {
}

NodABC::NodABC(const NodABC& nod) : Nod(nod),
    st(nod.st ? new NodABC(*nod.st) : nullptr),
    dr(nod.dr ? new NodABC(*nod.dr) : nullptr) {
}

NodABC::~NodABC() {
    delete st;
    delete dr;
    st = dr = nullptr;
}

NodABC& NodABC::operator=(const NodABC& nod) {
    if (this != &nod) {
        Nod::operator=(nod);

        delete st;
        delete dr;

        st = nod.st ? new NodABC(*nod.st) : nullptr;
        dr = nod.dr ? new NodABC(*nod.dr) : nullptr;
    }
    return *this;
}

int NodABC::inaltime() const {
    return 1 + std::max((st ? st->inaltime() : 0), (dr ? dr->inaltime() : 0));
}

int NodABC::nrNoduri() const {
    return 1 + (st ? st->nrNoduri() : 0) + (dr ? dr->nrNoduri() : 0);
}

void NodABC::afisareRSD(std::ostream& out) const {
    out << getInfo() << ' ';
    if (st) {
        st->afisareRSD(out);
    }
    if (dr) {
        dr->afisareRSD(out);
    }
}

void NodABC::afisareSRD(std::ostream& out) const {
    if (st) {
        st->afisareSRD(out);
    }
    out << getInfo() << ' ';
    if (dr) {
        dr->afisareSRD(out);
    }
}

void NodABC::afisareSDR(std::ostream& out) const {
    if (st) {
        st->afisareSDR(out);
    }
    if (dr) {
        dr->afisareSDR(out);
    }
    out << getInfo() << ' ';
}

bool NodABC::inserare(int info) {
    int nodInfo = getInfo();
    if (info < nodInfo) {
        // Trebuie inserat in subarborele stang
        if (st) {
            return st->inserare(info);
        } else {
            st = new NodABC(info);
            return true;
        }
    } else if (nodInfo < info) {
        // Trebuie inserat in subarborele drept
        if (dr) {
            return dr->inserare(info);
        } else {
            dr = new NodABC(info);
            return true;
        }
    } else {
        // Deja a fost inserat
        return false;
    }
}
