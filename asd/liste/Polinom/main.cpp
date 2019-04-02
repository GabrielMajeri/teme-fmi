#include <fstream>
#include <iostream>

using namespace std;

/// Un monom de un anumit grad
struct Monom {
    Monom* urm = NULL;
    int grad;
    int coef;

    /// Adauga un nou monom dupa acesta
    void adauga_succesor(Monom* m) {
        m->urm = urm;
        urm = m;
    }

    /// Construieste un nou monom cu aceeasi valoare,
    /// dar fara succesor
    Monom* copie() {
        Monom* p = new Monom;

        p->urm = NULL;
        p->grad = grad;
        p->coef = coef;

        return p;
    }
};

/// Polinom construit din monoame
struct Polinom {
    Monom* prim, * ultim;

    Polinom()
        : prim(NULL), ultim(NULL) {
    }

    /// Sterge toate monoamele
    ~Polinom() {
        Monom* m = prim;

        while (m != NULL) {
            Monom* urm = m->urm;
            delete m;
            m = urm;
        }

        prim = ultim = NULL;
    }

    Polinom(Polinom&& p) {
        *this = move(p);
    }

    Polinom& operator=(Polinom&& p) {
        prim = p.prim;
        ultim = p.ultim;
        p.prim = p.ultim = NULL;
        return *this;
    }

    /// Calculeaza valoarea unui polinom intr-un punct
    int valoare(int x) {
        int suma = 0;

        for (Monom* m = prim; m != NULL; m = m->urm) {
            int val = x;
            for (int i = 0; i < m->grad; ++i) {
                val *= x;
            }
            val *= m->coef;
            suma += val;
        }

        return suma;
    }

    /// Adauga un monom la final, presupunand ca are grad mai mare
    void insereaza_final(Monom* p) {
        if (!prim) {
            prim = ultim = p;
        }

        ultim->urm = p;
        ultim = p;
    }

    /// Citeste un polinom din fisier
    void citeste(ifstream& in) {
        int grad_max;
        in >> grad_max;

        for (int grad = 0; grad < grad_max; ++grad) {
            int coef;
            in >> coef;

            if (coef == 0) {
                continue;
            }

            Monom* m = new Monom;
            m->urm = NULL;
            m->grad = grad;
            m->coef = coef;

            insereaza_final(m);
        }
    }

    /// Afiseaza polinomul
    void afisare() {
        if (!prim) {
            cout << 0 << '\n';
            return;
        }

        cout << prim->coef << "*x^" << prim->grad;

        for (Monom* m = prim->urm; m != NULL; m = m->urm) {
            bool negativ = (m-> coef < 0);

            if (negativ) {
                cout << " - " << -m->coef;
            } else {
                cout << " + " << m->coef;
            }

            cout << "*x^" << m->grad;
        }

        cout << '\n';
    }

    /// Aduna acest polinom cu altul
    Polinom aduna(const Polinom& b) const {
        Polinom rezultat;
        Monom* pa = prim;
        Monom* pb = b.prim;

        while (pa != NULL && pb != NULL) {
            if (pa->grad < pb->grad) {
                Monom* m = new Monom;

                m->grad = pa->grad;
                m->coef = pa->coef;

                rezultat.insereaza_final(m);

                pa = pa->urm;
            } else if (pa->grad > pb->grad) {
                Monom* m = new Monom;

                m->grad = pb->grad;
                m->coef = pb->coef;

                rezultat.insereaza_final(m);

                pb = pb->urm;
            } else {
                int suma = pa->coef + pb->coef;

                if (suma != 0) {
                    Monom* m = new Monom;

                    m->grad = pa->grad;
                    m->coef = suma;

                    rezultat.insereaza_final(m);
                }

                pa = pa->urm;
                pb = pb->urm;
            }
        }

        while (pa != NULL) {
            rezultat.insereaza_final(pa->copie());
            pa = pa->urm;
        }

        while (pb != NULL) {
            rezultat.insereaza_final(pb->copie());
            pb = pb->urm;
        }

        return rezultat;
    }

    /// Inmulteste polinomul cu un monom
    Polinom produs_monom(const Monom* b) const {
        Polinom rezultat;

        for (Monom* m = prim; m != NULL; m = m->urm) {
            int produs =  m->coef * b->coef;

            if (produs == 0) {
                continue;
            }

            Monom* p = new Monom;

            p->coef = produs;
            p->grad = m->grad + b->grad;

            rezultat.insereaza_final(p);
        }

        return rezultat;
    }

    /// Inmulteste polinomul cu un scalar, adica un monom de grad 0
    Polinom produs_scalar(int s) const {
        Monom m;
        m.coef = s;
        m.grad = 0;

        return produs_monom(&m);
    }

    /// Inmulteste polinomul cu alt polinom
    Polinom produs(const Polinom& b) const {
        Polinom rezultat;

        for (Monom* m = b.prim; m != NULL; m = m->urm) {
            Polinom intermediar = produs_monom(m);

            rezultat = rezultat.aduna(intermediar);
        }

        return rezultat;
    }
};

int main() {
    Polinom a, b;

    ifstream in("polinom.in");
    a.citeste(in);
    b.citeste(in);


    cout << "a = ";
    a.afisare();

    cout << "b = ";
    b.afisare();


    int x;
    in >> x;

    cout << "\n";

    cout << "Valoarea lui a in x = " << x << " este " << a.valoare(x) << "\n";
    cout << "\n";


    int scalar;
    in >> scalar;

    Polinom p_scalar = a.produs_scalar(scalar);

    cout << "a * " << scalar << " = ";
    p_scalar.afisare();
    cout << "\n";


    Polinom suma = a.aduna(b);

    cout << "a + b = ";
    suma.afisare();
    cout << "\n";

    Polinom p = a.produs(b);

    cout << "a * b = ";
    p.afisare();
    cout << "\n";
}
