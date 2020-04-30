package exceptions.calculator;

public class OverflowException extends ArithmeticException {
    public OverflowException(String message) {
        super("numeric overflow: " + message);
    }
}
