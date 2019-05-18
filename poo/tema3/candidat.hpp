#pragma once

#include <iostream>
#include <string>

class Act;

class Candidat {
    std::string nume;
    Act* act;
    int numarDosar;

    static int contorDosare;

protected:
    double medieBac;
    double notaProba;

    Candidat(const std::string& nume, Act* act, double medieBac, double notaProba);

    std::string getNrDosar() const;

public:
    Candidat(const Candidat& rhs);
    virtual ~Candidat();

    Candidat& operator=(const Candidat& rhs);

    /// Calculeaza media de admitere
    virtual double medieAdmitere() const = 0;

    /// Returneaza codul legitimatiei de concurs
    virtual std::string codLegitimatie() const = 0;

    /// Afiseaza candidatul impreuna cu toate datele lui
    virtual void afisare(std::ostream& out) const;

    /// Returneaza o copie de acelasi tip cu acest candidat, alocata dinamic
    virtual Candidat* copie() const = 0;

    /// Returneaza true daca candidatul s-a inscris cu un pasaport,
    /// nu cu un buletin.
    bool arePasaport() const;

    // Compara doi candidati in ordinea depunerii dosarelor
    bool comparaDupaNumarDosar(const Candidat& rhs) const;
    bool comparaDupaMedie(const Candidat& rhs) const;

    friend std::ostream& operator<<(std::ostream& out, const Candidat& c);

    static Candidat* citesteCandidat(std::istream& in);
};

class CandidatIF : public virtual Candidat {
public:
    CandidatIF(const std::string& nume, Act* act, double medieBac, double notaProbaScrisa);


    double medieAdmitere() const override;
    std::string codLegitimatie() const override;
    void afisare(std::ostream& out) const override;
    CandidatIF* copie() const override;
};

class CandidatID : public virtual Candidat {
    double notaMate;

public:
    CandidatID(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, double notaMate);

    double medieAdmitere() const override;
    std::string codLegitimatie() const override;
    void afisare(std::ostream& out) const override;
    CandidatID* copie() const override;
};

class Candidat2Fac : public virtual Candidat {
    std::string programAbsolvit;
    double notaPrima;

public:
    Candidat2Fac(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, const std::string& programAbsolvit, double notaPrima);

    double medieAdmitere() const override;
    void afisare(std::ostream& out) const override;
};

class CandidatIF2 : public CandidatIF, public Candidat2Fac {
public:
    CandidatIF2(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, const std::string& programAbsolvit, double notaPrima);

    double medieAdmitere() const override;
    std::string codLegitimatie() const override;
    void afisare(std::ostream& out) const override;
    CandidatIF2* copie() const override;
};

class CandidatID2 : public CandidatID, public Candidat2Fac {
public:
    CandidatID2(const std::string& nume, Act* act, double medieBac, double notaProbaOrala, const std::string& programAbsolvit, double notaPrima);

    double medieAdmitere() const override;
    std::string codLegitimatie() const override;
    void afisare(std::ostream& out) const override;
    CandidatID2* copie() const override;
};
