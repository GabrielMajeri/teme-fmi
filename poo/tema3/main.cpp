#include <fstream>
#include <string>

#include "act.hpp"
#include "candidat.hpp"
#include "gestiune.hpp"

void citeste(std::istream& in) {
    int nrLocuriIF, nrLocuriID;

    std::cout << "Introduceti nr locuri IF:\n";
    in >> nrLocuriIF;
    std::cout << "Introduceti nr locuri ID:\n";
    in >> nrLocuriID;

    Gestiune<CandidatIF> frecv(nrLocuriIF);
    Gestiune<CandidatID> dist(nrLocuriID);

    while (true)
    {
        std::cout << "Introduceti optiunea dorita:\n";
        std::cout << "- 0 = nou candidat\n"
                << "- 1 = afiseaza candidati inscrisi\n"
                << "- 2 = afiseaza admisi\n"
                << "- 3 = afiseaza candidati cu pasaport\n"
                << "- orice alt caracter = iesi din program\n";

        int opt;
        in >> opt;

        if (!in) {
            return;
        }

        std::cout << '\n';

        switch (opt)
        {
        case 0:
        {
            Candidat* cnd = Candidat::citesteCandidat(in);

            CandidatIF* cndFrecv = dynamic_cast<CandidatIF*>(cnd);
            CandidatID* cndDist = dynamic_cast<CandidatID*>(cnd);

            if (cndFrecv) {
                frecv += cndFrecv;
            } else if (cndDist) {
                dist += cndDist;
            } else {
                throw std::runtime_error("candidat de tip necunoscut");
            }

            break;
        }
        case 1:
        {
            std::cout << "Situatie candidati:\n";
            std::cout << "-- Inscrisi la IF:\n";
            frecv.afisareInscrisi();
            std::cout << '\n';

            std::cout << "-- Inscrisi la ID:\n";
            dist.afisareInscrisi();
            std::cout << '\n';

            break;
        }
        case 2:
        {
            Gestiune<CandidatIF> frecv2 = frecv;
            Gestiune<CandidatID> dist2(dist);

            std::cout << "-- Admisi la IF:\n";
            frecv.afisareAdmisi();

            std::cout << "-- Admisi la ID:\n";
            dist2.afisareAdmisi();

            break;
        }
        case 3:
        {
            std::cout << "  Candidati la IF cu pasaport:\n";
            frecv.afisareCandidatiCuPasaport();
            std::cout << "\n";

            std::cout << "  Candidati la ID cu pasaport:\n";
            dist.afisareCandidatiCuPasaport();
            std::cout << "\n";

            break;
        }
        default:
            return;
        }

        std::cout << '\n';
    }
}

int main()
{
    try {
        std::ifstream in("date.txt");
        citeste(in);
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
}
