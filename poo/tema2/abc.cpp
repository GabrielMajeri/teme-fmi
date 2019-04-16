#include "abc.hpp"

const Nod* ABC::getRadacina() const {
    return rad;
}

ABC::ABC() : rad(nullptr) {
}

ABC::ABC(const ABC& abc)
    : Arbore(abc),
      rad(abc.rad ? new NodABC(*abc.rad) : nullptr) {
}

ABC::~ABC() {
    goleste();
}

void ABC::goleste() {
    delete rad;
    rad = nullptr;
}

bool ABC::inserare(int info) {
    if (!rad) {
        rad = new NodABC(info);
        return true;
    }

    return rad->inserare(info);
}

std::ostream& operator<<(std::ostream& out, const ABC& abc) {
    const NodABC* rad = abc.rad;

    if (!rad) {
        out << "(Arbore vid)";
        return out;
    }

    out << "SRD: ";
    rad->afisareSRD(out);
    out << '\n';

    out << "RSD: ";
    rad->afisareRSD(out);
    out << '\n';

    out << "SDR: ";
    rad->afisareSDR(out);
    out << '\n';

    out << "Inaltime: " << abc.inaltime() << '\n';
    out << "Nr. noduri: " << abc.nrNoduri() << '\n';

    return out;
}

std::istream& operator>>(std::istream& in, ABC& abc) {
    abc.goleste();

    int n;
    in >> n;

    for (int i = 0; i < n; ++i) {
        int info;
        in >> info;
        abc.inserare(info);
    }

    return in;
}
