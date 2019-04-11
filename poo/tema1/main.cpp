// Tema 12 - Matrice de numere complexe reprezentate ca structuri inlantuite

#include <fstream>
#include <iostream>

#include "matrix.hpp"

/// Verifica ca doua valori sunt egale, altfel iese cu un mesaj de eroare
template <typename T>
void assertEq(const T& lhs, const T& rhs, const char* message) {
    if (lhs != rhs) {
        std::cerr << message << ": " << lhs << " != " << rhs << "\n";
        std::exit(1);
    }
}

/// Testeaza clasa de numere complexe
void testComplex() {
    Complex a(3, 2);
    Complex b(-2, 1);

    assertEq(a.conjugate(), Complex(3, -2), "Complex conjugate");
    assertEq(a + b, Complex(1, 3), "Complex sum");
    assertEq(a - b, Complex(5, 1), "Complex difference");
    assertEq(a * b, Complex(-8, -1), "Complex product");
    assertEq(a / b, Complex(-0.8, -1.4), "Complex division");
}

/// Testeaza lista sortata simplu inlantuita
void testList() {
    SortedList<char> list;

    list.set(15, 'a');
    list.set(4, 'b');
    list.set(17, 'c');
    list.set(15, 'z');

    assertEq(*list.get(4), 'b', "List setter / getter");
    assertEq(list.get(-1), (char*)nullptr, "List get invalid element");
    assertEq(*list.get(15), 'z', "List overwrite");

    SortedList<char> copy;
    copy = list;

    SortedList<char> expectedCopy;
    expectedCopy.set(17, 'c');
    expectedCopy.set(15, 'z');
    expectedCopy.set(4, 'b');

    assertEq(copy, expectedCopy, "List operator=");

    list.erase(15);
    list.erase(17);
    list.erase(4);
    assertEq(list.empty(), true, "List erase");
}

/// Testeaza clasa de matrice de numere complexe
void testMatrix() {
    Matrix m1(5000, 10000);

    Complex z(5, -2);
    m1.setElement(5, 100, z);
    m1.setElement(5, 7800, z * 3.5);

    assertEq(m1.getElement(5, 100), z, "Matrix element setter / getter");

    m1.setElement(1000, 500, Complex(2.5, 1));

    Matrix m2(5000, 10000);

    m2.setElement(250, 42, Complex(1, 0));
    m2.setElement(5, 101, Complex(3, 5));
    m2.setElement(5, 100, Complex(3, 1));

    Matrix sum = m1 + m2;

    Matrix expectedSum(5000, 10000);
    expectedSum.setElement(5, 100, Complex(8, -1));
    expectedSum.setElement(5, 101, Complex(3, 5));
    expectedSum.setElement(5, 7800, Complex(17.5, -7));
    expectedSum.setElement(250, 42, Complex(1, 0));
    expectedSum.setElement(1000, 500, Complex(2.5, 1));

    assertEq(sum, expectedSum, "Matrix sum");

    Matrix m2T = m2.transpose();

    Matrix expectedTranspose(10000, 5000);
    expectedTranspose.setElement(42, 250, Complex(1, 0));
    expectedTranspose.setElement(100, 5, Complex(3, 1));
    expectedTranspose.setElement(101, 5, Complex(3, 5));

    assertEq(m2T, expectedTranspose, "Matrix transpose");

    Matrix product = m1 * m2T;

    Matrix expectedProduct(5000, 5000);
    expectedProduct.setElement(5, 5, Complex(17, -1));

    assertEq(product, expectedProduct, "Matrix product");

    Matrix nonSingular(2, 2);
    nonSingular.setElement(0, 0, Complex(1));
    nonSingular.setElement(0, 1, Complex(0, 2));
    nonSingular.setElement(1, 0, Complex(3));
    nonSingular.setElement(1, 1, Complex(0, 4));

    assertEq(nonSingular.determinant(), Complex(0, -2), "Matrix determinant");

    Matrix expectedInverse(2, 2);
    expectedInverse.setElement(0, 0, Complex(-2));
    expectedInverse.setElement(0, 1, Complex(1));
    expectedInverse.setElement(1, 0, Complex(0, -1.5));
    expectedInverse.setElement(1, 1, Complex(0, 0.5));

    assertEq(nonSingular.inverse(), expectedInverse, "Matrix inverse");
}

void runTests() {
    testComplex();
    testList();
    testMatrix();
}

/// Citeste din matrice din fisier,
/// o afiseaza si calculeaza inversa
void readMatrixFromFile() {
    std::ifstream in("matrix.in");

    Matrix m;
    in >> m;

    std::cout << "M = " << m << '\n';
    std::cout << "M^-1 = " << m.inverse() << '\n';
}

int main() {
    try {
        runTests();
        readMatrixFromFile();
    } catch (std::exception& exc) {
        std::cerr << "Exception: " << exc.what() << '\n';
        return 1;
    }
}
