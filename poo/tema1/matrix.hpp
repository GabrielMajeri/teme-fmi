#ifndef MATRIX_HPP
#define MATRIX_HPP

/*
clasa matrice sa fie prietena a clasei numar complex si sa contina metode pentru:

- determinantul unei matrici patratice, implementat tot cu ajutorul
sumei si produsului de numere complexe (cele care supraincarca + si * in clasa numar complex);

- inversa unei matrici patratice cu determinantul nenul, care sa foloseasca functiile descrise anterior;
*/

#include "complex.hpp"
#include "list.hpp"

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

    const Complex* getElement(int row, int column) const;
    void setElement(int row, int column, Complex value);

    /// Construieste matricea transpusa
    Matrix transpose() const;

    Matrix operator+(const Matrix& rhs) const;
    Matrix operator*(const Matrix& rhs) const;
    Matrix operator*(Complex rhs) const;

    friend std::ostream& operator<<(std::ostream& os, const Matrix& m);
    friend std::istream& operator>>(std::istream& is, Matrix& m);
};


#endif
