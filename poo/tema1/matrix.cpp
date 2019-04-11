#include "matrix.hpp"

Matrix::Matrix(int rowCount, int columnCount)
    : rows(), rowCount(rowCount), columnCount(columnCount) {
    if (rowCount < 0 || columnCount < 0) {
        throw std::invalid_argument("Matrix dimension is negative");
    }
}

bool Matrix::operator==(const Matrix& rhs) const {
    return rowCount == rhs.rowCount &&
        columnCount == rhs.columnCount &&
        rows == rhs.rows;
}

bool Matrix::operator!=(const Matrix& rhs) const {
    return !(*this == rhs);
}

Complex Matrix::getElement(int row, int column) const {
    if (row >= rowCount || column >= columnCount) {
        throw std::out_of_range("Element index out of bounds");
    }

    const Row* r = getRow(row);

    if (r) {
        const Complex* elem = r->get(column);

        if (elem) {
            return *elem;
        }
    }

    return Complex(0, 0);
}

void Matrix::setElement(int row, int column, Complex value) {
    if (row >= rowCount || column >= columnCount) {
        throw std::out_of_range("Element index out of bounds");
    }

    Row* r = getRow(row);

    if (r == nullptr) {
        rows.set(row, SortedList<Complex>());
        r = getRow(row);
    }

    if (value.isZero()) {
        r->erase(column);
    } else {
        r->set(column, value);
    }

    if (r->empty()) {
        rows.erase(row);
    }
}

bool Matrix::isSquare() const {
    return rowCount == columnCount;
}

Matrix Matrix::transpose() const {
    Matrix result(columnCount, rowCount);

    for (const Node<Row>* rowPtr = rows.cbegin(); rowPtr != nullptr; rowPtr = rowPtr->nextNode()) {
        for (const Node<Complex>* colPtr = rowPtr->getValue().cbegin(); colPtr != nullptr; colPtr = colPtr->nextNode()) {
            result.setElement(colPtr->getKey(), rowPtr->getKey(), colPtr->getValue());
        }
    }

    return result;
}

Matrix Matrix::minor(int row, int column) const {
    if (!isSquare()) {
        throw std::logic_error("Non square matrix minor");
    }

    Matrix result(rowCount - 1, columnCount - 1);

    for (const Node<Row>* rowPtr = rows.cbegin(); rowPtr != nullptr; rowPtr = rowPtr->nextNode()) {
        for (const Node<Complex>* colPtr = rowPtr->getValue().cbegin(); colPtr != nullptr; colPtr = colPtr->nextNode()) {
            int currentRow = rowPtr->getKey(), currentCol = colPtr->getKey();
            Complex currentValue = colPtr->getValue();
            if (currentRow < row) {
                if (currentCol < column) {
                    result.setElement(currentRow, currentCol, currentValue);
                } else if (column < currentCol) {
                    result.setElement(currentRow, currentCol - 1, currentValue);
                }
            } else if (row < currentRow) {
                if (currentCol < column) {
                    result.setElement(currentRow - 1, currentCol, currentValue);
                } else if (column < currentCol) {
                    result.setElement(currentRow - 1, currentCol - 1, currentValue);
                }
            }
        }
    }

    return result;
}

Complex Matrix::determinant() const {
    if (!isSquare()) {
        throw std::logic_error("Non square matrix determinant");
    }

    // Cazuri de baza
    if (rowCount == 0) {
        return Complex(0, 0);
    } else if (rowCount == 1) {
        return getElement(0, 0);
    } else if (rowCount == 2) {
        return getElement(0, 0) * getElement(1, 1) - getElement(0, 1) * getElement(1, 0);
    }

    const Row* first = getRow(0);

    if (first == nullptr) {
        return Complex(0, 0);
    }

    Complex det;

    for (const Node<Complex>* elem = first->cbegin(); elem != nullptr; elem = elem->nextNode()) {
        int column = elem->getKey();
        double sign = (column % 2 == 0) ? +1 : -1;

        det = det + elem->getValue() * sign * minor(0, column).determinant();
    }

    return det;
}

Matrix Matrix::inverse() const {
    if (!isSquare()) {
        throw std::logic_error("Non square matrix inverse");
    }

    Complex det = determinant();

    if (det.isZero()) {
        throw std::logic_error("Non invertible matrix");
    }

    Matrix result(rowCount, columnCount);

    for (int i = 0; i < rowCount; ++i) {
        for (int j = 0; j < columnCount; ++j) {
            double sign = ((i + j) % 2 == 0) ? 1 : -1;

            Complex z = minor(i, j).determinant() * sign;

            result.setElement(i, j, z);
        }
    }

    return result.transpose() * (Complex(1) / det);
}

Matrix Matrix::operator+(const Matrix& rhs) const {
    if (rowCount != rhs.rowCount || columnCount != rhs.columnCount) {
        throw std::invalid_argument("Matrix size mismatch");
    }

    Matrix result(rowCount, columnCount);

    const Node<Row>* pa = rows.cbegin();
    const Node<Row>* pb = rhs.rows.cbegin();

    while (pa && pb) {
        if (pa->getKey() < pb->getKey()) {
            result.rows.set(pa->getKey(), pa->getValue());
            pa = pa->nextNode();
        } else if (pa->getKey() > pb->getKey()) {
            result.rows.set(pb->getKey(), pb->getValue());
            pb = pb->nextNode();
        } else {
            int row = pa->getKey();

            const Node<Complex>* pca = pa->getValue().cbegin();
            const Node<Complex>* pcb = pb->getValue().cbegin();

            while (pca && pcb) {
                if (pca->getKey() < pcb->getKey()) {
                    result.setElement(row, pca->getKey(), pca->getValue());
                    pca = pca->nextNode();
                } else if (pca->getKey() > pcb->getKey()) {
                    result.setElement(row, pcb->getKey(), pcb->getValue());
                    pcb = pcb->nextNode();
                } else {
                    Complex sum = pca->getValue() + pcb->getValue();

                    if (!sum.isZero()) {
                        result.setElement(row, pca->getKey(), sum);
                    }

                    pca = pca->nextNode();
                    pcb = pcb->nextNode();
                }
            }

            while (pca) {
                result.setElement(row, pca->getKey(), pca->getValue());
                pca = pca->nextNode();
            }

            while (pcb) {
                result.setElement(row, pcb->getKey(), pcb->getValue());
                pcb = pcb->nextNode();
            }



            pa = pa->nextNode();
            pb = pb->nextNode();
        }
    }

    while (pa) {
        result.rows.set(pa->getKey(), pa->getValue());
        pa = pa->nextNode();
    }

    while (pb) {
        result.rows.set(pb->getKey(), pb->getValue());
        pb = pb->nextNode();
    }

    return result;
}

Matrix Matrix::operator*(const Matrix& rhs) const {
    if (columnCount != rhs.rowCount) {
        throw std::invalid_argument("Matrix size mismatch");
    }

    Matrix result(rowCount, rhs.columnCount);
    Matrix right = rhs.transpose();

    for (const Node<Row>* row = rows.cbegin(); row != nullptr; row = row->nextNode()) {
        for (const Node<Row>* rightCol = right.rows.cbegin(); rightCol != nullptr; rightCol = rightCol->nextNode()) {
            Complex sum;

            for (const Node<Complex>* col = row->getValue().cbegin(); col != nullptr; col = col->nextNode()) {
                const Complex* rightColValue = rightCol->getValue().get(col->getKey());

                if (rightColValue != nullptr) {
                    sum = sum + col->getValue() * (*rightColValue);
                }
            }

            if (!sum.isZero()) {
                result.setElement(row->getKey(), rightCol->getKey(), sum);
            }
        }
    }

    return result;
}

Matrix Matrix::operator*(Complex rhs) const {
    Matrix result(rowCount, columnCount);

    for (const Node<Matrix::Row>* prow = rows.cbegin(); prow != nullptr; prow = prow->nextNode()) {
        const SortedList<Complex>& row = prow->getValue();
        for (const Node<Complex>* pcol = row.cbegin(); pcol != nullptr; pcol = pcol->nextNode()) {
            result.setElement(prow->getKey(), pcol->getKey(), pcol->getValue() * rhs);
        }
    }

    return result;
}

std::ostream& operator<<(std::ostream& os, const Matrix& m) {
    if (m.rows.empty()) {
        return os << "(empty)\n";
    }

    for (const Node<Matrix::Row>* prow = m.rows.cbegin(); prow != nullptr; prow = prow->nextNode()) {
        const SortedList<Complex>& row = prow->getValue();
        os << "row " << prow->getKey() << ": ";
        for (const Node<Complex>* pcol = row.cbegin(); pcol != nullptr; pcol = pcol->nextNode()) {
            os << "(" << pcol->getKey() << ", " << pcol->getValue() << ") ";
        }
        os << '\n';
    }
    return os;
}

std::istream& operator>>(std::istream& is, Matrix& m) {
    is >> m.rowCount >> m.columnCount;

    int elements;
    is >> elements;

    for (int i = 0; i < elements; ++i) {
        int row, column;
        Complex value;
        is >> row >> column >> value;

        m.setElement(row, column, value);
    }

    return is;
}
