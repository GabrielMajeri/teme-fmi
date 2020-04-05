package calculator;

public class SimpleCalculator implements Calculator {
    @Override
    public double add(Double lhs, Double rhs) {
        if (lhs == null) {
            throw new IllegalArgumentException("left-hand side is null");
        }
        if (rhs == null) {
            throw new IllegalArgumentException("right-hand side is null");
        }

        double sum = lhs + rhs;

        if (sum == Double.POSITIVE_INFINITY) {
            throw new OverflowException("sum is +inf");
        } else if (sum == Double.NEGATIVE_INFINITY) {
            throw new UnderflowException("sum is -inf");
        }

        return sum;
    }

    @Override
    public double divide(Double dividend, Double divisor) {
        if (dividend == null) {
            throw new IllegalArgumentException("dividend is null");
        }
        if (divisor == null) {
            throw new IllegalArgumentException("divisor is null");
        }

        if (divisor == 0) {
            throw new DivideByZeroException("divisor is 0");
        }
        return dividend / divisor;
    }
}
