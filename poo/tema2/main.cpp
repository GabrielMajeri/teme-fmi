// Tema 11. Arbori oarecare, in reprezentare inlantuita.

#include <iostream>
#include <fstream>

#include "abc.hpp"
#include "arbore_oarecare.hpp"

int main() {
    std::ifstream in("arbore.in");

    ArboreOarecare a;
    in >> a;

    ArboreOarecare altul;
    altul = a;
    a.goleste();

    std::cout << "Arbore oarecare:\n";
    std::cout << altul;

    ABC abc;
    in >> abc;

    std::cout << "Arbore binar de cautare:\n";
    std::cout << abc;
}
