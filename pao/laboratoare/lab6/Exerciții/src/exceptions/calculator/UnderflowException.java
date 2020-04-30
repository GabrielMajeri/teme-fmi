package exceptions.calculator;

public class UnderflowException extends ArithmeticException {
    public UnderflowException(String message) {
        super("numeric underflow: " + message);
    }
}
