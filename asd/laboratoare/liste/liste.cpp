#include <iostream>
#include <fstream>
#include <cstring>

using namespace std;

/// Un nod din lista
struct Nod {
    Nod* urm;
    int info;
};

/// O lista simplu inlantuita
struct Lista {
    Nod* prim = nullptr;
    Nod* ultim = nullptr;

    /// Construieste o noua lista, initial fara elemente
    Lista() {}

    /// Sterge nodurile ramase
    ~Lista() {
        sterge_lista();
    }

    Lista(Lista&& l) {
        *this = move(l);
    }

    Lista& operator=(Lista&& l) {
        prim = l.prim;
        ultim = l.ultim;
        l.prim = l.ultim = nullptr;
        return *this;
    }

    /// Sterge toate nodurile din lista
    void sterge_lista() {
        while (prim != nullptr) {
            sterge_inceput();
        }
    }

    /// Insereaza un nou nod la sfarsit.
    /// Functioneaza si daca lista este goala
    void inserare_sfarsit(int valoare) {
        Nod* p = new Nod;

        p->urm = nullptr;
        p->info = valoare;

        if (ultim == nullptr) {
            prim = ultim = p;
        }

        ultim->urm = p;
        ultim = p;
    }

    /// Insereaza un nod, la inceputul listei.
    /// Functioneaza si daca lista este goala
    void inserare_inceput(int valoare) {
        Nod* p = new Nod;

        p->urm = prim;
        p->info = valoare;

        if (prim == nullptr) {
            ultim = p;
        }

        prim = p;
    }

    /// Insereaza un nod, dupa nodul pointat de `p`.
    void inserare_dupa(Nod* p, int valoare) {
        Nod* nou_urm = p->urm ? p->urm->urm : nullptr;

        Nod* nou_nod = new Nod;
        nou_nod->info = valoare;
        nou_nod->urm = nou_urm;

        p->urm = nou_nod;

        if (p == ultim) {
            ultim = p->urm;
        }
    }

    /// Insereaza o valoare dupa nodul de pe pozitia `k`
    void inserare_dupa_pozitie(int k, int valoare) {
        Nod* p = cauta_pozitie(k);

        if (p == nullptr) {
            cerr << "inserare pe pozitie inexistenta\n";
            return;
        }

        inserare_dupa(p, valoare);
    }

    /// Afiseaza toata lista
    void afisare() const {
        for (Nod* p = prim; p != nullptr; p = p->urm) {
            cout << p->info << ' ';
        }
        cout << '\n';
    }

    /// Cauta primul nod cu valoarea `val`.
    /// Returneaza null daca nu gaseste.
    Nod* cauta_valoare(int val) {
        for (Nod* p = prim; p != nullptr; p = p->urm) {
            if (p->info == val) {
                return p;
            }
        }

        return nullptr;
    }

    /// Cauta nodul de pe pozitia `k`.
    /// Returneaza null daca nu exista.
    Nod* cauta_pozitie(int k) {
        int i = 0;
        for (Nod* p = prim; p != nullptr; p = p->urm, ++i) {
            if (i == k) {
                return p;
            }
        }

        return nullptr;
    }

    /// Sterge primul nod
    void sterge_inceput() {
        Nod* p = prim;
        // Trecem la urmatorul
        prim = prim->urm;
        delete p;
    }

    /// Sterge nodul pointat de `p`
    void sterge_nod(Nod* p) {
        // Daca este primul
        if (p == prim) {
            sterge_inceput();
        } else if (p == ultim) {
            // Penultimul nod devine ultimul
            Nod* penultim = prim;

            for (; penultim->urm != ultim; penultim = penultim->urm)
                ;

            ultim = penultim;
            penultim->urm = nullptr;
        } else {
            // Daca este in mijloc
            Nod* urm = p->urm;
            p->info = urm->info;
            p->urm = urm->urm;
            delete urm;
        }
    }

    /// Sterge toate nodurile cu valoarea `val`
    void sterge_valoare(int val) {
        Nod* p = cauta_valoare(val);
        while (p != nullptr) {
            sterge_nod(p);
            p = cauta_valoare(val);
        }
    }

    /// Sterge nodul de pe pozitia `k`
    void sterge_pozitie(int k) {
        Nod* p = cauta_pozitie(k);
        if (p) {
            sterge_nod(p);
        }
    }
};

/// Construieste o lista
Lista exemplu_lista() {
    // 3 -1 2 4 5 6 3
    Lista l;

    l.inserare_inceput(3);
    l.inserare_sfarsit(-1);
    l.inserare_sfarsit(2);
    l.inserare_sfarsit(4);

    l.inserare_dupa_pozitie(3, 5);

    l.inserare_sfarsit(6);
    l.inserare_sfarsit(3);

    return l;
}

/// Teste pentru pb. 1
void testeaza() {
    Lista l = exemplu_lista();

    cout << "Lista este: ";
    l.afisare();

    l.sterge_valoare(3);

    l.sterge_pozitie(-1);

    cout << "4 se afla pe " << (l.cauta_pozitie(4)->info) << '\n';
}

/// Problema 2: numarul de nr. pare si media aritmetica a nr. impare
void problema2(const Lista& l) {
    int nr_pare = 0;

    int suma_impare = 0;
    int nr_impare = 0;

    for (Nod* p = l.prim; p != nullptr; p = p->urm) {
        int info = p->info;
        if (info % 2 == 0) {
            nr_pare += 1;
        } else {
            suma_impare += info;
            nr_impare += 1;
        }
    }

    cout << "Lista contine " << nr_pare << " numere pare\n";

    double medie = (double)(suma_impare) / nr_impare;
    cout << "Media aritmetica a nr impare este " << medie << '\n';
}

/// Problema 3: intre fiecare doua elemente inseram media lor aritmetica
void problema3(Lista& l) {
    Nod* p = l.prim;

    // pana la penultimul element
    while (p->urm != nullptr) {
        Nod* st = p;
        Nod* dr = p->urm;

        Nod* nou = new Nod;
        nou->info = (st->info + dr->info) / 2;
        nou->urm = dr;

        p->urm = nou;

        p = dr;
    }

    l.afisare();
}

/// Citeste un numar mare dintr-o linie din fisier
Lista citeste_numar_mare(ifstream& in) {
    Lista l;

    char sir[1024];
    in.getline(sir, 1024);

    // Construiesc lista
    for (int n = strlen(sir) - 1; n >= 0; --n) {
        int cifra = sir[n] - '0';
        l.inserare_sfarsit(cifra);
    }

    return l;
}

/// Aduna numerele mari `a` si `b`
Lista suma_numere(const Lista& a, const Lista& b) {
    Lista suma;

    Nod* pa = a.prim;
    Nod* pb = b.prim;

    int carry = 0;
    while (pa != nullptr && pb != nullptr) {
        int sum_cif = pa->info + pb->info;

        if (carry) {
            carry = 0;
            sum_cif += 1;
        }

        if (sum_cif >= 10) {
            carry = 1;
            sum_cif = sum_cif % 10;
        }

        suma.inserare_sfarsit(sum_cif);

        pa = pa->urm;
        pb = pb->urm;
    }

    Nod* mai_lung = pa != nullptr ? pa : pb;

    while (mai_lung != nullptr) {
        int cif = mai_lung->info;

        if (carry) {
            cif += carry;
            carry = 0;
        }
        if (cif >= 10) {
            carry = 1;
            cif = cif % 10;
        }

        suma.inserare_sfarsit(cif);

        mai_lung = mai_lung->urm;
    }

    if (carry) {
        suma.inserare_sfarsit(carry);
        carry = 0;
    }

    return suma;
}

/// Inmulteste un numar mare cu o cifra
Lista produs_numar_cifra(const Lista& nr, int cif) {
    Lista produs;

    Nod* p = nr.prim;

    int carry = 0;
    while (p != nullptr) {
        int prod_cif = p->info * cif;

        if (carry) {
            prod_cif += carry;
            carry = 0;
        }

        if (prod_cif > 9) {
            carry = prod_cif / 10;
            prod_cif = prod_cif % 10;
        }

        produs.inserare_sfarsit(prod_cif);

        p = p->urm;
    }

    if (carry) {
        produs.inserare_sfarsit(carry);
    }

    return produs;
}

/// Inmulteste doua numere mari
Lista produs_numere(const Lista& a, const Lista& b) {
    Lista produs;

    Nod* p = b.prim;

    int k = 0;

    while (p != nullptr) {
        int cif = p->info;

        Lista partial = produs_numar_cifra(a, cif);

        // Inmultesc un numar mare cu 10^k
        for (int i = 0; i < k; ++i) {
            partial.inserare_inceput(0);
        }

        produs = suma_numere(produs, partial);

        ++k;
        p = p->urm;
    }

    return produs;
}

int main() {
    cout << "Testez problema 1\n";
    testeaza();
    cout << "\n";

    Lista l = exemplu_lista();

    cout << "Problema 2\n";
    problema2(l);
    cout << "\n";

    cout << "Problema 3\n";
    problema3(l);
    cout << "\n";

    ifstream in("liste.in");

    cout << "Problema 4\n";

    Lista a = citeste_numar_mare(in);
    Lista b = citeste_numar_mare(in);

    cout << " a    = ";
    a.afisare();
    cout << " b    = ";
    b.afisare();

    cout << "a + b = ";
    Lista suma = suma_numere(a, b);
    suma.afisare();

    cout << "\n";

    cout << "Problema 5\n";

    cout << "a * b = ";
    Lista produs = produs_numere(a, b);
    produs.afisare();
}
