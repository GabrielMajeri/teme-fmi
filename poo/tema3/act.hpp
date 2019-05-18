#pragma once

#include <iostream>
#include <string>

class Act {
protected:
    int numar;

    // Construieste un nou act de identificare
    Act(int nr);

    /// Calculeaza numarul de cifre ale unui intreg
    static int numarCifre(int nr);

public:
    virtual ~Act();
    virtual void afisare(std::ostream& out) const = 0;

    /// Citeste tipul de act al candidatului si datele din act
    static Act* citeste(std::istream& in);

    friend std::ostream& operator<<(std::ostream& out, const Act& act);
};

class CI : public Act {
    // Seria de buletin
    std::string serie;

public:
    CI(const std::string& serie, int numar);

    void afisare(std::ostream& out) const override;
};

class Pasaport : public Act {
public:
    Pasaport(int numar);

    void afisare(std::ostream& out) const override;
};
