#include <iostream>
#include <fstream>

#include <algorithm>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

struct Nod;

using NodPtr = std::unique_ptr<Nod>;

struct Nod {
    unsigned frecv;
    char ch;
    NodPtr st, dr;

    Nod(char ch, unsigned frecv)
        : frecv(frecv), ch(ch), st(nullptr), dr(nullptr) {
    }

    Nod(NodPtr&& st, NodPtr&& dr)
        : frecv(st->frecv + dr->frecv), ch(0), st(std::move(st)), dr(std::move(dr)) {
    }
};

NodPtr construieste_arbore(std::ifstream& in) {
    std::vector<NodPtr> v;

    int n;
    in >> n;

    auto compare = [] (const NodPtr& lhs, const NodPtr& rhs) { return lhs->frecv > rhs->frecv; };

    for (int i = 0; i < n; ++i) {
        char ch;
        unsigned frecv;
        in >> ch >> frecv;

        v.emplace_back(std::make_unique<Nod>(ch, frecv));
    }

    std::make_heap(v.begin(), v.end(), compare);

    for (int i = 0; i < n - 1; ++i) {
        std::pop_heap(v.begin(), v.end(), compare);
        NodPtr st = std::move(v.back());
        v.pop_back();

        std::pop_heap(v.begin(), v.end(), compare);
        NodPtr dr = std::move(v.back());
        v.pop_back();

        NodPtr c = std::make_unique<Nod>(std::move(st), std::move(dr));

        v.push_back(std::move(c));
        std::push_heap(v.begin(), v.end(), compare);
    }

    return std::move(v.back());
}

using Coduri = std::unordered_map<char, std::string>;

void genereaza_coduri(const NodPtr& nod, std::string cod, Coduri& coduri) {
    if (nod->st) {
        genereaza_coduri(nod->st, cod + '0', coduri);
    }

    if (nod->dr) {
        genereaza_coduri(nod->dr, cod + '1', coduri);
    }

    if (nod->ch != 0) {
        coduri.emplace(nod->ch, cod);
    }
}

std::string codifica(const Coduri& coduri, std::string sir) {
    std::vector<char> v;

    for (auto ch : sir) {
        const auto& cod = coduri.at(ch);

        v.reserve(v.size() + cod.size());
        v.insert(v.end(), cod.cbegin(), cod.cend());
    }

    return std::string(v.data(), v.size());
}

std::string decodifica(const NodPtr& arbore, std::string sir) {
    std::string rezultat = "";

    auto it = sir.cbegin();

    while (it != sir.cend()) {
        Nod* p = arbore.get();

        while (p->ch == 0) {
            char urm = *it++;
            if (urm == '0') {
                p = p->st.get();
            } else {
                p = p->dr.get();
            }

            if (p == NULL) {
                std::cerr << "Cod Huffman invalid\n";
                exit(1);
            }
        }

        rezultat += p->ch;
    }

    return rezultat;
}

int main() {
    std::ifstream in("huffman.in");

    if (!in) {
        std::cerr << "Nu pot deschide fisierul de intrare\n";
        return 1;
    }

    NodPtr arbore = construieste_arbore(in);

    std::unordered_map<char, std::string> coduri;
    genereaza_coduri(arbore, "", coduri);

    int n;
    in >> n;

    for (int i = 0; i < n; ++i) {
        std::string sir;
        in >> sir;

        auto codificat = codifica(coduri, sir);

        std::cout << codificat << '\n';
    }

    in >> n;

    for (int i = 0; i < n; ++i) {
        std::string sir;
        in >> sir;

        std::cout << decodifica(arbore, sir) << '\n';
    }
}
