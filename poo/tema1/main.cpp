// Tema 12 - Matrice de numere complexe reprezentate ca structuri inlantuite

#include <iostream>
#include <sstream>

#include "complex.hpp"
#include "matrix.hpp"

/// Verifica ca doua valori sunt egale, altfel iese cu un mesaj de eroare
template <typename T>
void assertEq(T lhs, T rhs, const std::string& message) {
    if (lhs != rhs) {
        std::cerr << message << ": " << lhs << " != " << rhs << "\n";
        std::exit(1);
    }
}

void testComplex() {
    Complex a(3, 2);
    Complex b(-2, 1);

    assertEq(a.conjugate(), Complex(3, -2), "Complex conjugate");
    assertEq(a + b, Complex(1, 3), "Complex sum");
    assertEq(a - b, Complex(5, 1), "Complex difference");
    assertEq(a * b, Complex(-8, -1), "Complex product");
    assertEq(a / b, Complex(-0.8, -1.4), "Complex division");

    std::stringstream os;
    os << a;
    assertEq(os.str(), std::string("(3 + 2i)"), "Complex printing");

    std::stringstream is("2.25 -5.75");
    Complex z;
    is >> z;
    assertEq(z, Complex(2.25, -5.75), "Complex parsing");
}

void testList() {
    SortedList<char> list;

    list.set(15, 'a');
    list.set(4, 'b');
    list.set(17, 'c');
    list.set(15, 'z');

    assertEq(*list.get(4), 'b', "List setter / getter");
    assertEq<char*>(list.get(-1), nullptr, "List get invalid element");
    assertEq(*list.get(15), 'z', "List overwrite");

    list.erase(15);
    list.erase(17);
    list.erase(4);
    assertEq(list.empty(), true, "List erasure");
}

void testMatrix() {
    Matrix m1(5000, 10000);

    Complex z(5, -2);
    m1.setElement(5, 100, z);
    m1.setElement(5, 7800, z * 3.5);

    assertEq(*m1.getElement(5, 100), z, "Matrix element setter / getter");

    m1.setElement(150, 20, Complex(2, 34));
    m1.setElement(150, 20, Complex());
    assertEq<const Complex*>(m1.getElement(150, 20), nullptr, "Matrix erase");

    m1.setElement(1000, 500, Complex(2.5, 1));

    Matrix m2(5000, 10000);

    m2.setElement(250, 42, Complex(1, 0));
    m2.setElement(5, 101, Complex(3, 5));
    m2.setElement(5, 100, Complex(3, 1));

    Matrix sum = m1 + m2;

    std::cout << "M1 = " << m1 << '\n';
    std::cout << "M2 = " << m2 << '\n';
    std::cout << "M1 + M2 = " << sum << '\n';

    std::cout << "M2^T = " << m2.transpose() << '\n';
    std::cout << "M1 * M2^T = " << (m1 * m2.transpose()) << '\n';
}

void runTests() {
    testComplex();
    testList();
    testMatrix();
}

int main() {
    runTests();
}
