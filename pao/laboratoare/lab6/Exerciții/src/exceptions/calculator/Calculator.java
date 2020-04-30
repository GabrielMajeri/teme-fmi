package exceptions.calculator;

public interface Calculator {
    /**
     * Adds two numbers and returns their sum.
     *
     * @param lhs left-hand side of the term
     * @param rhs right-hand side of the term
     * @return the sum `lhs + rhs`
     */
    double add(Double lhs, Double rhs) throws OverflowException, UnderflowException;

    /**
     * Divides a number by another and returns the quotient.
     *
     * @param dividend the number to divide
     * @param divisor the number to divide by
     * @return the quotient `dividend / divisor`
     */
    double divide(Double dividend, Double divisor);

    /**
     * Computes the average of a vector of numbers.
     *
     * @param values the list of numbers
     * @return the arithmetic mean of the numbers
     */
    default double average(Double[] values) {
        double sum = 0;
        for (Double value : values) {
            if (value == null) {
                throw new NullPointerException("Double in vector is null");
            }
            sum = add(sum, value);
        }
        return divide(sum, (double) values.length);
    }
}
