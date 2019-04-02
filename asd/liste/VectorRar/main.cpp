#include <iostream>
#include <fstream>

using namespace std;

struct Element {
    Element* urm;
    int index;
    int val;
};

struct VectorRar {
    Element* prim, * ultim;

    VectorRar()
        : prim(nullptr), ultim(nullptr) {
    }

    ~VectorRar() {
        while (prim != nullptr) {
            Element* urm = prim->urm;
            delete prim;
            prim = urm;
        }
        prim = ultim = nullptr;
    }


    VectorRar(VectorRar&& v)
        : prim(v.prim), ultim(v.ultim) {
        v.prim = v.ultim = nullptr;
    }

    VectorRar& operator=(VectorRar&& v) {
        prim = v.prim;
        ultim = v.ultim;
        v.prim = v.ultim = nullptr;
        return *this;
    }

    void adauga(int index, int val) {
        Element* e = new Element;
        e->index = index;
        e->val = val;
        e->urm = nullptr;

        if (!prim) {
            prim = ultim = e;
            return;
        }

        if (index < prim->index) {
            e->urm = prim;
            prim = e;
            return;
        }

        Element* p;
        for (p = prim; p->urm != nullptr; p = p->urm) {
            if (p->urm->index > index) {
                break;
            }
        }

        e->urm = p->urm;
        p->urm = e;

        if (p == ultim) {
            ultim = p->urm;
        }
    }

    void adauga_final(int index, int val) {
        Element* e = new Element;
        e->index = index;
        e->val = val;
        e->urm = nullptr;

        if (!prim) {
            prim = ultim = e;
            return;
        }

        ultim->urm = e;
        ultim = e;
    }

    void citire(ifstream& in) {
        int n;
        in >> n;
        for (int i = 0; i < n; ++i) {
            int index, val;
            in >> index >> val;
            adauga(index, val);
        }
    }

    void afisare() const {
        for (Element* e = prim; e != NULL; e = e->urm) {
            cout << "(" << e->index << ", " << e->val << ")" << ' ';
        }
        cout << '\n';
    }

    VectorRar suma(const VectorRar& b) const {
        VectorRar rezultat;

        Element* pa = prim;
        Element* pb = b.prim;

        while (pa && pb) {
            if (pa->index < pb->index) {
                rezultat.adauga_final(pa->index, pa->val);
                pa = pa->urm;
            } else if (pa->index > pb->index) {
                rezultat.adauga_final(pb->index, pb->val);
                pb = pb->urm;
            } else {
                int suma = pa->val + pb->val;

                if (suma != 0) {
                    rezultat.adauga_final(pa->index, suma);
                }

                pa = pa->urm;
                pb = pb->urm;
            }
        }

        while (pa) {
            rezultat.adauga_final(pa->index, pa->val);
            pa = pa->urm;
        }

        while (pb) {
            rezultat.adauga_final(pb->index, pb->val);
            pb = pb->urm;
        }

        return rezultat;
    }

    VectorRar produs(const VectorRar& b) {
        VectorRar rezultat;

        Element* pa = prim;
        Element* pb = b.prim;

        while (pa && pb) {
            if (pa->index < pb->index) {
                pa = pa->urm;
            } else if (pa->index > pb->index) {
                pb = pb->urm;
            } else {
                int produs = pa->val * pb->val;

                rezultat.adauga_final(pa->index, produs);

                pa = pa->urm;
                pb = pb->urm;
            }
        }

        return rezultat;
    }
};

int main()
{
    VectorRar a, b;

    ifstream in("rar.in");

    a.citire(in);
    b.citire(in);

    cout << "a = ";
    a.afisare();
    cout << "b = ";
    b.afisare();
    cout << "\n";

    VectorRar c = a.suma(b);

    cout << "a + b = ";
    c.afisare();
    cout << "\n";

    VectorRar d = a.produs(b);
    cout << "a * b = ";
    d.afisare();
}
