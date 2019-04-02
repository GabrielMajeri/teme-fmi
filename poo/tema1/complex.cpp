#include "complex.hpp"

Complex::Complex(double real, double imag)
    : re(real), im(imag) {
}

Complex Complex::conjugate() const {
    return Complex(re, -im);
}

double Complex::absSquared() const {
    return (re * re) + (im * im);
}

bool Complex::isZero() const {
    return absSquared() == 0;
}

bool Complex::operator==(Complex rhs) const {
    return re == rhs.re && im == rhs.im;
}

bool Complex::operator!=(Complex rhs) const {
    return !(*this == rhs);
}

Complex Complex::operator+(Complex rhs) const {
    return Complex(re + rhs.re, im + rhs.im);
}

Complex Complex::operator-(Complex rhs) const {
    return Complex(re - rhs.re, im - rhs.im);
}

Complex Complex::operator*(double rhs) const {
    return Complex(re * rhs, im * rhs);
}

Complex Complex::operator/(double rhs) const {
    if (rhs == 0.0) {
        throw std::invalid_argument("Division by zero");
    }

    return Complex(re / rhs, im / rhs);
}

Complex Complex::operator*(Complex rhs) const {
    //   (a + bi)(c + di) =
    // = ac + adi + bci + bdi^2 =
    // = (ac - bd) + (ad + bc)i
    double real = re * rhs.re - im * rhs.im;
    double imag = re * rhs.im + im * rhs.re;
    return Complex(real, imag);
}

Complex Complex::operator/(Complex rhs) const {
    //   (a + bi)/(c + di) =
    // = ((a + bi)(c - di))/((c + di)(c - di)) =
    // = ((a + bi)(c - di))/(c^2 + d^2)
    return ((*this) * rhs.conjugate()) / rhs.absSquared();
}

std::ostream& operator<<(std::ostream& os, const Complex& z) {
    return os << "(" << z.re << " + " << z.im << "i)";
}

std::istream& operator>>(std::istream& is, Complex& z) {
    is >> z.re >> z.im;
    return is;
}
