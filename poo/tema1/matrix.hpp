#ifndef MATRIX_HPP
#define MATRIX_HPP

#include "complex.hpp"
#include "list.hpp"

/// Matrice rara de numere complexe implementata folosind
/// liste simplu inlantuite.
class Matrix {
    using Row = SortedList<Complex>;
    SortedList<Row> rows;

    int rowCount, columnCount;

    Row* getRow(int row) {
        return rows.get(row);
    }

    const Row* getRow(int row) const {
        return rows.get(row);
    }

public:
    /// Construieste o noua matrice rara cu dimensiunile date
    Matrix(int rowCount = 0, int columnCount = 0);

    bool operator==(const Matrix& rhs) const;
    bool operator!=(const Matrix& rhs) const;

    Complex getElement(int row, int column) const;
    void setElement(int row, int column, Complex value);

    /// Verifica daca matricea este patratica
    bool isSquare() const;

    /// Construieste matricea transpusa
    Matrix transpose() const;

    /// Returneaza un minor al matricei
    Matrix minor(int row, int column) const;

    /// Calculeaza determinantul matricei
    Complex determinant() const;

    /// Calculeaza inversul matricei
    Matrix inverse() const;

    Matrix operator+(const Matrix& rhs) const;
    Matrix operator*(const Matrix& rhs) const;
    Matrix operator*(Complex rhs) const;

    friend std::ostream& operator<<(std::ostream& os, const Matrix& m);
    friend std::istream& operator>>(std::istream& is, Matrix& m);
};


#endif
