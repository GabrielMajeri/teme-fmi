#include "candidat.hpp"

#include "act.hpp"

int Candidat::contorDosare = 0;

Candidat::Candidat(const std::string& nume, Act* act, double medieBac, double notaProba)
    : nume(nume), act(act), numarDosar(++contorDosare), medieBac(medieBac), notaProba(notaProba) {
}

std::string Candidat::getNrDosar() const {
    return std::to_string(numarDosar);
}

Candidat::Candidat(const Candidat& rhs)
    : nume(rhs.nume), numarDosar(rhs.numarDosar), medieBac(rhs.medieBac),
    notaProba(rhs.notaProba) {
    CI* ci = dynamic_cast<CI*>(rhs.act);
    Pasaport* pp = dynamic_cast<Pasaport*>(rhs.act);

    if (ci) {
        act = new CI(*ci);
    } else if (pp) {
        act = new Pasaport(*pp);
    } else {
        throw std::runtime_error("tip de act necunoscut");
    }
}

Candidat::~Candidat() {
    delete act;
    act = nullptr;
}

Candidat& Candidat::operator=(const Candidat& rhs) {
    if (this == &rhs) {
        return *this;
    }

    nume = rhs.nume;
    numarDosar = rhs.numarDosar;
    medieBac = rhs.medieBac;
    notaProba = rhs.notaProba;

    delete act;
    act = nullptr;

    CI* ci = dynamic_cast<CI*>(rhs.act);
    Pasaport* pp = dynamic_cast<Pasaport*>(rhs.act);

    if (ci) {
        act = new CI(*ci);
    } else if (pp) {
        act = new Pasaport(*pp);
    } else {
        throw std::runtime_error("tip de act necunoscut");
    }

    return *this;
}

void Candidat::afisare(std::ostream& out) const {
    out << "Candidat " << nume << " (" << codLegitimatie() << ")\n";
    out << "Act: " << *act << '\n';
    out << "Medie admitere: " << medieAdmitere() << '\n';
    out << "- Medie bac: " << medieBac << '\n';
}

bool Candidat::arePasaport() const {
    return dynamic_cast<Pasaport*>(act) != nullptr;
}

bool Candidat::comparaDupaNumarDosar(const Candidat& rhs) const {
    return numarDosar < rhs.numarDosar;
}

bool Candidat::comparaDupaMedie(const Candidat& rhs) const {
    return medieAdmitere() > rhs.medieAdmitere();
}

std::ostream& operator<<(std::ostream& out, const Candidat& c) {
    c.afisare(out);
    return out;
}

Candidat* Candidat::citesteCandidat(std::istream& in) {
    std::cout << "Introduceti datele candidatului:\n";

    std::cout << "Nume candidat:\n";
    std::string nume;
    in.get();
    std::getline(in, nume);

    std::cout << "Este la a doua facultate (0 = nu, 1 = da):\n";
    bool fac2;
    in >> fac2;

    std::cout << "Introduceti datele pentru actul de identitate:\n";
    Act* act = Act::citeste(in);

    std::cout << "Introduceti media la bac:\n";
    double medieBac;
    in >> medieBac;

    std::cout << "Introduceti sectia (0 = IF, 1 = ID):\n";
    int sectie;
    in >> sectie;

    double notaProba;
    if (sectie == 0) {
        std::cout << "Introduceti nota la proba scrisa:\n";
    } else {
        std::cout << "Introduceti nota la proba orala:\n";
    }
    in >> notaProba;

    std::string programAbsolvit;
    double notaPrima;
    double notaMate;

    if (fac2) {
        std::cout << "Introduceti numele primei facultati:\n";
        in.get();
        std::getline(in, programAbsolvit);

        std::cout << "Introduceti nota de la prima facultate:\n";
        in >> notaPrima;
    } else {
        if (sectie == 1) {
            std::cout << "Introduceti nota la mate la bac:\n";
            in >> notaMate;
        }
    }

    Candidat* cnd;

    if (sectie == 0) {
        if (fac2) {
            cnd = new CandidatIF2(nume, act, medieBac, notaProba, programAbsolvit, notaPrima);
        } else {
            cnd = new CandidatIF(nume, act, medieBac, notaProba);
        }
    } else if (sectie == 1) {
        if (fac2) {
            cnd = new CandidatID2(nume, act, medieBac, notaProba, programAbsolvit, notaPrima);
        } else {
            cnd = new CandidatID(nume, act, medieBac, notaProba, notaMate);
        }
    } else {
        throw std::runtime_error("sectie necunoscuta: " + std::to_string(sectie));
    }

    return cnd;
}

// Invatamant cu frecventa

CandidatIF::CandidatIF(const std::string& nume, Act* act, double medieBac, double notaProbaScrisa)
    : Candidat(nume, act, medieBac, notaProbaScrisa) {}

double CandidatIF::medieAdmitere() const {
    return 0.8 * notaProba + 0.2 * medieBac;
}

std::string CandidatIF::codLegitimatie() const {
    return "IF_" + getNrDosar();
}

void CandidatIF::afisare(std::ostream& out) const {
    Candidat::afisare(out);
    out << "- Nota proba scrisa: " << notaProba << '\n';
}

CandidatIF* CandidatIF::copie() const {
    return new CandidatIF(*this);
}

// Invatamant la distanta

CandidatID::CandidatID(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, double notaMate)
    : Candidat(nume, act, medieBac, notaProbaOrala), notaMate(notaMate) {}

double CandidatID::medieAdmitere() const {
    return 0.6 * notaProba + 0.4 * notaMate;
}

std::string CandidatID::codLegitimatie() const {
    return "ID_" + getNrDosar();
}

void CandidatID::afisare(std::ostream& out) const {
    Candidat::afisare(out);
    out << "- Nota proba orala: " << notaProba << '\n';
    out << "- Nota mate bac: " << notaMate << '\n';
}

CandidatID* CandidatID::copie() const {
    return new CandidatID(*this);
}

// A doua facultate

Candidat2Fac::Candidat2Fac(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, const std::string& programAbsolvit, double notaPrima)
    : Candidat(nume, act, medieBac, notaProbaOrala), programAbsolvit(programAbsolvit), notaPrima(notaPrima) {
}

double Candidat2Fac::medieAdmitere() const {
    return 0.6 * notaProba + 0.4 * notaPrima;
}

void Candidat2Fac::afisare(std::ostream& out) const {
    Candidat::afisare(out);
    out << "- Facultate absolvita: " << programAbsolvit << " cu media " << notaPrima << '\n';
}

// A doua facultate - Invatamant cu frecventa

CandidatIF2::CandidatIF2(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, const std::string& programAbsolvit, double notaPrima)
    : Candidat(nume, act, medieBac, notaProbaOrala),
    CandidatIF(nume, act, medieBac, notaProbaOrala),
    Candidat2Fac(nume, act, medieBac, notaProbaOrala, programAbsolvit, notaPrima) {
}

double CandidatIF2::medieAdmitere() const {
    return Candidat2Fac::medieAdmitere();
}

std::string CandidatIF2::codLegitimatie() const {
    return "IF2_" + getNrDosar();
}

void CandidatIF2::afisare(std::ostream& out) const {
    Candidat2Fac::afisare(out);
}

CandidatIF2* CandidatIF2::copie() const {
    return new CandidatIF2(*this);
}

// A doua facultate - Invatamant la distanta

CandidatID2::CandidatID2(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, const std::string& programAbsolvit, double notaPrima)
    : Candidat(nume, act, medieBac, notaProbaOrala),
    CandidatID(nume, act, medieBac, notaProbaOrala, 0.0),
    Candidat2Fac(nume, act, medieBac, notaProbaOrala, programAbsolvit, notaPrima) {
}

double CandidatID2::medieAdmitere() const {
    return Candidat2Fac::medieAdmitere();
}

std::string CandidatID2::codLegitimatie() const {
    return "ID2_" + getNrDosar();
}

void CandidatID2::afisare(std::ostream& out) const {
    Candidat2Fac::afisare(out);
}

CandidatID2* CandidatID2::copie() const {
    return new CandidatID2(*this);
}
