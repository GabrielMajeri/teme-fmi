#pragma once

#include <algorithm>
#include <vector>

template <typename T>
class Gestiune {
    int nrLocuri;
    std::vector<T*> candidati;

public:
    /// Construieste o noua clasa care gestioneaza candidatii la o sectie
    Gestiune(int nrLocuri);

    Gestiune(const Gestiune& rhs);
    ~Gestiune();

    Gestiune& operator=(const Gestiune& rhs);

    /// Adauga un nou candidat la aceasta sectie
    void operator+=(T* candidat);

    /// Afiseaza candidatii de la aceasta sectie, in ordinea inscrierii
    void afisareInscrisi();

    /// Afiseaza candidatii admisi la aceasta sectie
    void afisareAdmisi();

    /// Afiseaza candidatii care s-au inscris cu pasaport
    void afisareCandidatiCuPasaport() const;
};

template <typename T>
Gestiune<T>::Gestiune(int nrLoc) : nrLocuri(nrLoc) {}

template <typename T>
Gestiune<T>::Gestiune(const Gestiune& rhs) : nrLocuri(rhs.nrLocuri) {
    for (T* candidat : rhs.candidati) {
        candidati.push_back(candidat->copie());
    }
}

template <typename T>
Gestiune<T>::~Gestiune() {
    for (T* candidat : candidati) {
        delete candidat;
    }
}

template <typename T>
Gestiune<T>& Gestiune<T>::operator=(const Gestiune& rhs) {
    if (this == &rhs) {
        return *this;
    }

    for (T* cnd : candidati) {
        delete cnd;
    }
    candidati.clear();

    for (T* cnd : rhs.candidati) {
        candidati.push_back(cnd->copie());
    }

    nrLocuri = rhs.nrLocuri;

    return *this;
}

template <typename T>
void Gestiune<T>::operator+=(T* candidat) {
    candidati.push_back(candidat);
}

template <typename T>
bool comparaCronologic(const T* lhs, const T* rhs) {
    return lhs->comparaDupaNumarDosar(*rhs);
}

template <typename T>
void Gestiune<T>::afisareInscrisi() {
    std::sort(candidati.begin(), candidati.end(), comparaCronologic<T>);
    for (T* candidat : candidati) {
        std::cout << *candidat << '\n';
    }
}

template <typename T>
bool comparaMedie(const T* lhs, const T* rhs) {
    return lhs->comparaDupaMedie(*rhs);
}

template <typename T>
void Gestiune<T>::afisareAdmisi() {
    std::sort(candidati.begin(), candidati.end(), comparaMedie<T>);
    int i = 0;
    for (T* candidat : candidati) {
        if (++i > nrLocuri) {
            break;
        }
        std::cout << *candidat << '\n';
    }
}

template <typename T>
void Gestiune<T>::afisareCandidatiCuPasaport() const {
    for (T* candidat : candidati) {
        if (candidat->arePasaport()) {
            std::cout << *candidat << '\n';
        }
    }
}
