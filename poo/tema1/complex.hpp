#ifndef COMPLEX_HPP
#define COMPLEX_HPP

#include <istream>

class Complex {
    double re, im;
public:
    /// Constructor pentru un nr. complex
    Complex(double real = 0.0, double imag = 0.0);

    /// Returneaza conjugatul nr. complex
    Complex conjugate() const;

    /// Returneaza patratul modulului nr. complex
    double absSquared() const;

    /// Verifica daca acest nr. complex este nul
    bool isZero() const;

    bool operator==(Complex rhs) const;
    bool operator!=(Complex rhs) const;

    Complex operator+(Complex rhs) const;
    Complex operator-(Complex rhs) const;
    Complex operator*(double rhs) const;

    Complex operator/(double rhs) const;
    Complex operator*(Complex rhs) const;
    Complex operator/(Complex rhs) const;

    friend std::ostream& operator<<(std::ostream& os, const Complex& z);
    friend std::istream& operator>>(std::istream& is, Complex& z);
};

#endif
