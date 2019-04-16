#include "arbore.hpp"

Arbore::~Arbore() = default;

int Arbore::inaltime() const {
    return getRadacina()->inaltime();
}

int Arbore::nrNoduri() const {
    return getRadacina()->nrNoduri();
}
