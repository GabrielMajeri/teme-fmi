#include <iostream>

using namespace std;

struct Nod {
    Nod* urm;
    int val;
};

struct ListaSortata {
    Nod* prim = NULL;

    ~ListaSortata() {
        while (prim != NULL) {
            Nod* urm = prim->urm;
            delete prim;
            prim = urm;
        }
        prim = NULL;
    }

    void adauga(int val) {
        Nod* nou = new Nod;
        nou->urm = NULL;
        nou->val = val;

        if (prim == NULL) {
            prim = nou;
            return;
        }

        if (val < prim->val) {
            nou->urm = prim;
            prim = nou;
            return;
        }

        Nod* p;
        int k = 0;
        for (p = prim; p->urm != NULL; p = p->urm) {
            ++k;
            if (p->urm->val > val) {
                break;
            }
        }

        nou->urm = p->urm;
        p->urm = nou;
    }

    void afiseaza() {
        for (Nod* p = prim; p != NULL; p = p->urm) {
            cout << p->val << ' ';
        }
        cout << '\n';
    }
};

int main() {
    ListaSortata l;

    l.adauga(5);
    l.adauga(-1);
    l.adauga(3);
    l.adauga(12);
    l.adauga(6);
    l.adauga(7);
    l.adauga(-2);

    l.afiseaza();
}
