// Majeri Constantin-Gabriel
// Grupa 132
// Compilator: g++ -std=c++11

#include <algorithm>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <typeinfo>
#include <vector>
#include <set>

using namespace std;


class Adresa {
    string strada;
    int numar;
    int sector;

public:
    Adresa();

    const string& getStrada() const;
    int getNr() const;
    int getSector() const;

    bool operator==(const Adresa& rhs) const;

    friend istream& operator>>(istream& in, Adresa& ad);
};

Adresa::Adresa() : strada(), numar(1), sector(1) {}

const string& Adresa::getStrada() const {
    return strada;
}

int Adresa::getNr() const {
    return numar;
}

int Adresa::getSector() const {
    return sector;
}

bool Adresa::operator==(const Adresa& rhs) const {
    return strada == rhs.strada && numar == rhs.numar && sector == rhs.sector;
}

istream& operator>>(istream& in, Adresa& ad) {
    cout << "strada = ";
    in >> ad.strada;
    cout << "numar = ";
    in >> ad.numar;
    if (ad.numar <= 0) {
        throw out_of_range("Numar strada invalid");
    }
    cout << "sector = ";
    in >> ad.sector;
    if (ad.sector < 1 || ad.sector > 6) {
        throw out_of_range("Sector invalid");
    }
    cout << '\n';

    return in;
}

ostream& operator<<(ostream& out, const Adresa& ad) {
    out << "Strada " << ad.getStrada()
        << " nr. " << ad.getNr()
        << ", sector " << ad.getSector();

    return out;
}

class Statie {
    static int contorStatii;
    Adresa adresa;
    const int cod;
    string nume;
    set<int> mijloace;

public:
    Statie();
    virtual ~Statie();

    virtual string getTip() const = 0;
    string getCodIntern() const;

    const string& getNume() const;
    const Adresa& getAdresa() const;

    bool areMijlocDeTransport(int nr) const;

    bool linieDirecta(const Statie& st) const;

    friend ostream& operator<<(ostream& out, const Statie& s);
    static Statie* citesteStatie(istream& in);
};

int Statie::contorStatii = 1;

Statie::Statie() : adresa(), cod(contorStatii++), nume(), mijloace() {}

Statie::~Statie() = default;

string Statie::getCodIntern() const {
    return getTip() + "-" + to_string(cod);
}

const string& Statie::getNume() const {
    return nume;
}

const Adresa& Statie::getAdresa() const {
    return adresa;
}

bool Statie::areMijlocDeTransport(int nr) const {
    return mijloace.count(nr) > 0;
}

bool Statie::linieDirecta(const Statie& st) const {
    for (auto mij : mijloace) {
        if (st.mijloace.count(mij) > 0) {
            return true;
        }
    }
    return false;
}

ostream& operator<<(ostream& out, const Statie& s) {
    out << "Statia " << s.getCodIntern() << ", " <<
        s.nume << " la adresa " << s.adresa;
    return out;
}

class StatieUrbana : public Statie {
public:
    string getTip() const;
};

string StatieUrbana::getTip() const {
    return "SU";
}


class StatieExtra : public Statie {
public:
    string getTip() const;
};

string StatieExtra::getTip() const {
    return "SE";
}


Statie* Statie::citesteStatie(istream& in) {
    cout << "Introduceti tip statie (1 = urbana, 2 = extraurbana):\n";
    int tip;
    in >> tip;

    Statie* s;
    if (tip == 1) {
        s = new StatieUrbana();
    } else if (tip == 2) {
        s = new StatieExtra();
    } else {
        throw runtime_error("Tip necunoscut de statie");
    }

    cout << "Introduceti numele statiei:\n";
    in >> s->nume;

    cout << "Introduceti adresa statiei:\n";
    in >> s->adresa;

    cout << "Introduceti nr. mijloacelor de transport:\n";
    int n;
    in >> n;

    cout << "Introduceti cele " << n << " mijloace de transport:\n";
    for (int i = 0; i < n; ++i) {
        int nr;
        in >> nr;
        s->mijloace.insert(nr);
    }

    return s;
}


class Gestiune {
    vector<Statie*> statii;

    void elibereaza();
    void copiaza(const Gestiune& g);

    Statie* gasesteStatieNume(const string& nume) const;

public:
    Gestiune();
    Gestiune(const Gestiune& g);
    ~Gestiune();

    Gestiune& operator=(const Gestiune& g);

    Gestiune& operator+=(Statie* s);

    void afisareStatieNume(const string& nume) const;
    void afisareStatieAdresa(const Adresa& ad) const;
    void afisareStatieCod(const string& cod) const;

    void afisareStatiiMijloc(int nr) const;

    double pretStatii(const string& nume1, const string& nume2) const;
};

void Gestiune::elibereaza() {
    for (auto s: statii) {
        delete s;
    }
    statii.clear();
}

void Gestiune::copiaza(const Gestiune& g) {
    for (auto s : g.statii) {
        Statie* copie = nullptr;
        StatieUrbana* su = dynamic_cast<StatieUrbana*>(s);
        StatieExtra* se = dynamic_cast<StatieExtra*>(s);

        if (su) {
            copie = new StatieUrbana(*su);
        } else if (se) {
            copie = new StatieExtra(*se);
        } else {
            throw runtime_error("Tip necunoscut de statie");
        }

        statii.push_back(copie);
    }
}

Statie* Gestiune::gasesteStatieNume(const string& nume) const {
    for (auto st : statii) {
        if (st->getNume() == nume) {
            return st;
        }
    }
    return nullptr;
}

Gestiune::Gestiune() : statii() {}

Gestiune::Gestiune(const Gestiune& g) : statii() {
    copiaza(g);
}

Gestiune::~Gestiune() {
    elibereaza();
}

Gestiune& Gestiune::operator=(const Gestiune& g) {
    if (this == &g) {
        return *this;
    }

    elibereaza();
    copiaza(g);

    return *this;
}

Gestiune& Gestiune::operator+=(Statie* s) {
    statii.push_back(s);
    return *this;
}

void Gestiune::afisareStatieNume(const string& nume) const {
    Statie* st = gasesteStatieNume(nume);
    if (st) {
        cout << *st << '\n';
    } else {
        cout << "Nu exista statie cu numele " << nume << '\n';
    }
}

void Gestiune::afisareStatieAdresa(const Adresa& ad) const {
    for (const auto& st : statii) {
        if (st->getAdresa() == ad) {
            cout << *st << '\n';
            return;
        }
    }
    cout << "Nu exista statie la adresa " << ad << '\n';
}

void Gestiune::afisareStatieCod(const string& cod) const {
    for (const auto& st : statii) {
        if (st->getCodIntern() == cod) {
            cout << *st << '\n';
            return;
        }
    }
    cout << "Nu exista statie cu cod " << cod << '\n';
}

void Gestiune::afisareStatiiMijloc(int mijloc) const {
    for (const auto& st : statii) {
        if (st->areMijlocDeTransport(mijloc)) {
            cout << *st << '\n';
            return;
        }
    }
    cout << "Nu exista statii pentru mijloc de transport " << mijloc << '\n';
}

double Gestiune::pretStatii(const string& nume1, const string& nume2) const {
    Statie* st1 = gasesteStatieNume(nume1);
    Statie* st2 = gasesteStatieNume(nume2);

    if (st1 == nullptr) {
        throw runtime_error("Nu exista statia " + nume1);
    } else if (st2 == nullptr) {
        throw runtime_error("Nu exista statia " + nume2);
    }

    double pret = 2.0;

    bool directa = st1->linieDirecta(*st2);
    bool urbana1 = typeid(*st1) == typeid(StatieUrbana);
    bool urbana2 = typeid(*st2) == typeid(StatieUrbana);

    if (urbana1 && urbana2) {
        if (!directa) {
            pret += 0.15 * pret;
        }
    } else if (!urbana1 && !urbana2) {
        if (directa) {
            pret += 0.2 * pret;
        } else {
            pret += 0.25 * pret;
        }
    } else {
        if (directa) {
            pret += 0.3 * pret;
        } else {
            pret += 0.4 * pret;
        }
    }

    return pret;
}

int main() {
    try {
        // citire de la tastatura
        istream& in = cin;

        Gestiune g;

        bool inMeniu = true;
        while (inMeniu) {
            cout << "Introduceti optiunea:\n"
                << "1 - adauga statie\n"
                << "2 - afisare statie (dupa nume, adresa sau cod)\n"
                << "3 - afisare statie dupa mijloc de transport\n"
                << "4 - estimare pret\n"
                << "orice altceva - iesi din program\n";

            int optiune;
            in >> optiune;
            switch (optiune) {
            case 1:
                {
                    Statie* st = Statie::citesteStatie(in);
                    g += st;
                }
                break;
            case 2:
                {
                    cout << "Dupa ce doriti sa afisati statia:\n"
                        << "1 - dupa nume\n"
                        << "2 - dupa adresa\n"
                        << "3 - dupa cod\n"
                        << "orice altceva - iesi\n";
                    in >> optiune;

                    switch (optiune) {
                    case 1:
                        {
                            cout << "Introduceti numele:\n";
                            string nume;
                            in >> nume;

                            g.afisareStatieNume(nume);
                        }
                        break;
                    case 2:
                        {
                            cout << "Introduceti adresa:\n";
                            Adresa ad;
                            in >> ad;

                            g.afisareStatieAdresa(ad);
                        }
                        break;
                    case 3:
                        {
                            cout << "Introduceti cod:\n";
                            string cod;
                            in >> cod;

                            g.afisareStatieCod(cod);
                        }
                        break;
                    default:
                        break;
                    }
                }
                break;

            case 3:
                {
                    cout << "Introduceti nr. mijlocului de transport:\n";
                    int mijloc;
                    in >> mijloc;

                    g.afisareStatiiMijloc(mijloc);
                }
                break;
            case 4:
                {
                    cout << "Introduceti numele celor 2 statii:\n";

                    string nume1, nume2;
                    in >> nume1 >> nume2;

                    cout << "Pret estimat: " << g.pretStatii(nume1, nume2) << '\n';
                }
                break;
            default:
                inMeniu = false;
                break;
            }
            cout << '\n';
        }

    } catch (const std::exception& e) {
        cerr << e.what() << '\n';
    }
}
