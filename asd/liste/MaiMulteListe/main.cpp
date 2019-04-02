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
    Nod* prim;
    Nod* ultim;

    /// Construieste o noua lista, initial fara elemente
    Lista() : prim(nullptr), ultim(nullptr) {}

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
        } else {
            ultim->urm = p;
            ultim = p;
        }
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

    /// Afiseaza toata lista
    void afisare() const {
        for (Nod* p = prim; p != nullptr; p = p->urm) {
            cout << p->info << ' ';
        }
        cout << '\n';
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
};

/// Construieste o noua lista, inversata
Lista inverseaza1(const Lista& l) {
    Lista noua;

    for (Nod* p = l.prim; p != NULL; p = p->urm) {
        noua.inserare_inceput(p->info);
    }

    return noua;
}

/// Inverseaza lista `l`
void inverseaza2(Lista& l) {
    Nod* prec = nullptr;
    Nod* p = l.prim;

    l.ultim = l.prim;

    while (p) {
        Nod* urm = p->urm;

        p->urm = prec;

        prec = p;
        p = urm;
    }

    l.prim = prec;
}

void problema1() {
    Lista l;

    l.inserare_sfarsit(1);
    l.inserare_sfarsit(3);
    l.inserare_sfarsit(6);
    l.inserare_sfarsit(-1);
    l.inserare_sfarsit(16);
    l.inserare_sfarsit(8);

    cout << "Lista initiala = ";
    l.afisare();

    Lista invers = inverseaza1(l);
    cout << "Lista noua, inversata = ";
    invers.afisare();

    inverseaza2(l);
    cout << "Lista inversata pe loc = ";
    l.afisare();

    cout << "\n";
}

/// Interclaseaza `a` si `b` si pune rezultatul in `a`.
void interclasare(Lista& a, Lista& b) {
    Nod* pa = a.prim, * pb = b.prim;

    Nod* prec = nullptr;
    while (pa && pb) {
        if (pa->info < pb->info) {
            // `pa` este deja pe pozitia corecta, avansez
            prec = pa;
            pa = pa->urm;
        } else if (pa->info > pb->info) {
            Nod* b_urm = pb->urm;

            pb->urm = pa;
            if (prec != nullptr) {
                prec->urm = pb;
            } else {
                a.prim = pb;
            }

            prec = pb;
            pb = b_urm;
        } else {
            Nod* a_urm = pa->urm;
            Nod* b_urm = pb->urm;

            pa->urm = pb;
            pb->urm = a_urm;
            prec = pb;

            if (a_urm == nullptr) {
                a.ultim = pb;
            }

            pa = a_urm;
            pb = b_urm;
        }
    }

    while (pb) {
        a.ultim->urm = pb;
        a.ultim = pb;
        pb = pb->urm;
    }

    b.prim = b.ultim = nullptr;
}

void problema2() {
    Lista a, b;

    a.inserare_sfarsit(1);
    a.inserare_sfarsit(3);
    a.inserare_sfarsit(4);
    a.inserare_sfarsit(6);
    a.inserare_sfarsit(9);
    a.afisare();

    b.inserare_sfarsit(-1);
    b.inserare_sfarsit(2);
    b.inserare_sfarsit(5);
    b.inserare_sfarsit(5);
    b.inserare_sfarsit(7);
    b.afisare();

    cout << "Lista interclasata = ";
    interclasare(a, b);

    a.afisare();
    cout << "\n";
}

/// Distribuie elementele din `l` in alte doua liste
void distrib(Lista& l, Lista& impar, Lista& par) {
    Nod* p = l.prim;

    impar.prim = impar.ultim = p;
    par.prim = par.ultim = p->urm;

    p = p->urm->urm;

    int k = 1;
    while (p) {
        Nod* urm = p->urm;
        p->urm = nullptr;

        if (k % 2 == 1) {
            impar.ultim->urm = p;
            impar.ultim = p;
        } else {
            par.ultim->urm = p;
            par.ultim = p;
        }

        p = urm;
        ++k;
    }

    l.prim = l.ultim = nullptr;
}

void problema3() {
    Lista c;

    c.inserare_sfarsit(1);
    c.inserare_sfarsit(5);
    c.inserare_sfarsit(6);
    c.inserare_sfarsit(0);
    c.inserare_sfarsit(2);
    c.inserare_sfarsit(3);

    cout << "C = ";
    c.afisare();

    Lista a, b;
    distrib(c, a, b);

    cout << "A = ";
    a.afisare();
    cout << "B = ";
    b.afisare();

    cout << "\n";
}

int main() {
    problema1();
    problema2();
    problema3();
}
