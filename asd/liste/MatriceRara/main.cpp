#include <iostream>
#include <fstream>

using namespace std;

struct Element {
    Element* urm;
    int coloana;
    int val;
};

struct Rand {
    Rand* urm;
    int rand;
    Element* prim;

    ~Rand() {
        while (prim) {
            Element* urm = prim->urm;
            delete prim;
            prim = urm;
        }
    }

    void adauga_element(int col, int val) {
        Element* nou = new Element;

        nou->urm = nullptr;
        nou->coloana = col;
        nou->val = val;

        if (prim == nullptr) {
            prim = nou;
            return;
        }

        if (col < prim->coloana) {
            nou->urm = prim;
            prim = nou;
            return;
        }

        Element* p = prim;
        for (; p->urm != nullptr; p = p->urm) {
            if (p->urm->coloana > col) {
                break;
            }
        }

        nou->urm = p->urm;
        p->urm = nou;
    }

    void afisare() {
        for (Element* e = prim; e != nullptr; e = e->urm) {
            cout << "(" << rand << ", " << e->coloana << ") = " << e->val << ' ';
        }
    }
};

struct Matrice {
    Rand* prim;

    Matrice() : prim(nullptr) {}

    ~Matrice() {
        while (prim) {
            Rand* urm = prim->urm;
            delete prim;
            prim = urm;
        }
    }

    Matrice(Matrice&& m) : prim(m.prim) {
        m.prim = nullptr;
    }

    Matrice& operator=(Matrice&& m) {
        prim = m.prim;
        m.prim = nullptr;

        return *this;
    }

    void citeste(ifstream& in) {
        int nr;
        in >> nr;

        for (int i = 0; i < nr; ++i) {
            int rand, coloana, val;
            in >> rand >> coloana >> val;
            adauga_element(rand, coloana, val);
        }
    }

    void adauga_element(int rand, int col, int val) {
        if (prim == nullptr) {
            Rand* nou = new Rand;

            nou->urm = nullptr;
            nou->rand = rand;
            nou->prim = nullptr;

            nou->adauga_element(col, val);

            prim = nou;
            return;
        }

        if (rand < prim->rand) {
            Rand* nou = new Rand;

            nou->urm = prim;
            nou->rand = rand;
            nou->prim = nullptr;

            nou->adauga_element(col, val);

            prim = nou;

            return;
        }

        Rand* p = prim;
        for (; p->urm != nullptr; p = p->urm) {
            if (p->urm->rand >= rand) {
                break;
            }
        }

        if (p->rand == rand) {
            p->adauga_element(col, val);
        } else {
            Rand* nou = new Rand;

            nou->urm = p->urm;
            nou->rand = rand;
            nou->prim = nullptr;

            nou->adauga_element(col, val);

            p->urm = nou;
        }
    }

    void afisare() {
        for (Rand* r = prim; r != nullptr; r = r->urm) {
            r->afisare();
            cout << '\n';
        }
        cout << '\n';
    }
};

Rand* adauga_rand(Rand*& p, int rand) {
    Rand* nou = new Rand;
    nou->rand = rand;
    nou->prim = nullptr;

    if (p) {
        nou->urm = p->urm;
        p->urm = nou;
    } else {
        nou->urm = nullptr;
        p = nou;
    }

    return nou;
}

Matrice aduna(const Matrice& a, const Matrice& b) {
    Matrice rez;
    rez.prim = nullptr;

    Rand* pa = a.prim, * pb = b.prim;
    Rand* prez = nullptr;

    while (pa && pb) {
        Rand* nou = new Rand;
        nou->prim = nullptr;
        nou->urm = nullptr;

        int nr_elem_rand = 0;

        if (pa->rand < pb->rand) {
            nou->rand = pa->rand;

            for (Element* e = pa->prim; e != nullptr; e = e->urm) {
                nou->adauga_element(e->coloana, e->val);
                ++nr_elem_rand;
            }

            pa = pa->urm;
        } else if (pa->rand > pb->rand) {
            nou->rand = pb->rand;

            for (Element* e = pb->prim; e != nullptr; e = e->urm) {
                nou->adauga_element(e->coloana, e->val);
                ++nr_elem_rand;
            }

            pb = pb->urm;
        } else {
            nou->rand = pa->rand;

            Element* ea = pa->prim, * eb = pb->prim;

            while (ea && eb) {
                if (ea->coloana < eb->coloana) {
                    nou->adauga_element(ea->coloana, ea->val);
                    ++nr_elem_rand;
                    ea = ea->urm;
                } else if (ea->coloana > eb->coloana) {
                    nou->adauga_element(eb->coloana, eb->val);
                    ++nr_elem_rand;
                    eb = eb->urm;
                } else {
                    int suma = ea->val + eb->val;
                    if (suma != 0) {
                        ++nr_elem_rand;
                        nou->adauga_element(ea->coloana, suma);
                    }
                    ea = ea->urm;
                    eb = eb->urm;
                }
            }

            while (ea) {
                prez->adauga_element(ea->coloana, ea->val);
                ++nr_elem_rand;
                ea = ea->urm;
            }

            while (eb) {
                prez->adauga_element(eb->coloana, eb->val);
                ++nr_elem_rand;
                eb = eb->urm;
            }

            pa = pa->urm;
            pb = pb->urm;
        }

        if (nr_elem_rand == 0) {
            delete nou;
            continue;
        }

        if (prez) {
            prez->urm = nou;
        } else {
            rez.prim = nou;
        }

        prez = nou;
    }

    while (pa) {
        Rand* nou = new Rand;
        nou->prim = nullptr;
        nou->rand = pa->rand;

        for (Element* e = pa->prim; e != nullptr; e = e->urm) {
            nou->adauga_element(e->coloana, e->val);
        }

        if (prez) {
            prez->urm = nou;
        } else {
            rez.prim = nou;
        }

        prez = nou;

        pa = pa->urm;
    }

    while(pb) {
        Rand* nou = new Rand;
        nou->prim = nullptr;
        nou->rand = pb->rand;

        for (Element* e = pb->prim; e != nullptr; e = e->urm) {
            nou->adauga_element(e->coloana, e->val);
        }

        if (prez) {
            prez->urm = nou;
        } else {
            rez.prim = nou;
        }

        prez = nou;


        pb = pb->urm;
    }

    return rez;
}

int main() {
    ifstream in("matrice.in");

    Matrice a, b;

    a.citeste(in);
    a.afisare();
    b.citeste(in);
    b.afisare();

    Matrice c;
    c = aduna(a, b);

    c.afisare();
}
