#include "nod.hpp"

#include <utility>

Nod::Nod(int valoare) : info(valoare) {
}

Nod::~Nod() = default;

int Nod::getInfo() const {
    return info;
}
