#include "act.hpp"

#include <stdexcept>

Act::Act(int nr)
    : numar(nr) {}

Act::~Act() = default;

int Act::numarCifre(int nr) {
    int k = 0;
    while (nr) {
        ++k;
        nr /= 10;
    }
    return k;
}

Act* Act::citeste(std::istream& in) {
    std::cout << "Introduceti tip de act (0 = buletin, 1 = pasaport):\n";
    int tip;
    in >> tip;

    if (tip == 0) {
        std::cout << "Seria si numarul de buletin:\n";
        std::string serie;
        int numar;
        in >> serie >> numar;
        return new CI(serie, numar);
    } else if (tip == 1) {
        std::cout << "Numarul de pasaport:\n";
        int numar;
        in >> numar;
        return new Pasaport(numar);
    } else {
        throw std::runtime_error("tip necunoscut de act: " + std::to_string(tip));
    }
}

std::ostream& operator<<(std::ostream& out, const Act& act) {
    act.afisare(out);
    return out;
}

CI::CI(const std::string& serie, int numar)
    : Act(numar), serie(serie) {
    if (serie.size() != 2) {
        throw std::invalid_argument("seria de buletin are lungimea gresita");
    }
    if (numarCifre(numar) != 6) {
        throw std::invalid_argument("numarul de buletin are lungimea gresita");
    }
}

void CI::afisare(std::ostream& out) const {
    out << "Buletin (seria " << serie << ", numar " << numar << ")";
}

Pasaport::Pasaport(int numar)
    : Act(numar) {
    if (numarCifre(numar) != 8) {
        throw std::invalid_argument("numarul de pasaport are lungimea gresita");
    }
}

void Pasaport::afisare(std::ostream& out) const {
    out << "Pasaport (numar " << numar << ")";
}
